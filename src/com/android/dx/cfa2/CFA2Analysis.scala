package com.android.dx.cfa2

import com.android.dx
import dx.opt.{OptimizationContext => Context, _}
import dx.dex.file.{ClassDefItem, ClassDataItem => CDI}
import dx.rop.cst.CstType
import dx.rop.`type`.Prototype
import wrap.{BasicBlock => BB, _}
import ROpCodes.{Val => _, _}
import Method._
import CFA2Analysis._
import Logger._
import tlc.Algebra._
import tlc.BOOL._
import env._
import `var`._
import `val`._
import Type._
import Exceptionals._
import prop.{Set => PropSet, _}
import collection._
import collection.{parallel => par}
import collection.JavaConversions._
import annotation._
import util.control.TailCalls.{Call=>TCall, _}
import util.control.Breaks._
import actors.Actor.actor
import scala.actors.Futures._
import scala.annotation.unchecked.uncheckedStable
import java.io.File

object CFA2Analysis {
  
  abstract class Opts protected[CFA2Analysis] extends Immutable with NotNull {
    import Opts._
    def single_threaded: Boolean
    def debug: Boolean
    // In seconds
    def timeout: Int
    def recursionFuel: Int
    def loopingFuel: Int
    def outPath: String
    def continueOnOverflow = !debug
    
    lazy val extra_classpaths: Option[List[java.net.URL]] = None
    protected[CFA2Analysis] lazy val starting_points: Iterable[MethodIDer] = immutable.Seq(MethodIDer.Accessible)
    
    protected[this] def mkLog(sym: Symbol): (Symbol, Logger) = (sym, new FileLogger(outPath+"/"+sym.name+".log"))
    protected[this] def mkLog(sym: Symbol, primary: Logger): (Symbol, Logger) =
      (sym, new DualLogger(primary, mkLog(sym)._2))
      
    protected[this] def _logs = immutable.Map(
      mkLog('out, new PrintLogger(System.out)),
      mkLog('info, new PrintLogger(System.err)),
      mkLog('warn, new PrintLogger(System.err)),
      ('debug, new ConditionalLogger(debug, new CompressedFileLogger(outPath+"/debug.log"))),
      mkLog('error, new PrintLogger(System.err)))
    final lazy val logs = _logs
    lazy val log = new Opts.Loggers(logs)
    def loggers = log.logs.values
  }
  object Opts {
    final class Loggers(val logs: Map[Symbol, Logger]) {
      def apply(l: Symbol) = logs(l)
    }
  }
  
  type EncodedTrace = scala.Array[Int]
  type TracerRepr[E] = immutable.Vector[E]
  final class Tracer[E](val self: TracerRepr[E])
  extends SeqProxyLike[E, TracerRepr[E]] with Immutable with NotNull with Serializable {
    def dominates(t:Tracer[E]) : Boolean =
      if(t.length > length) false
      else this endsWith(t)
    
    /** Trace from after last occurrence of bb */
    def header(e:E) = {
      require(contains(e))
      slice(lastIndexOf(e), length)
    }
    lazy val preheader = if(!isEmpty) slice(lastIndexOf(last, length-2), length-1)
                         else         immutable.Vector.empty // TODO: Abstract using TracerRepr
    
    def encode : EncodedTrace = {
      val encoded = new mutable.ArrayBuilder.ofInt()
      for(e <- this)
        encoded += e.hashCode
      encoded result
    }
    // For SeqProxyLike
    def seq = self
    protected[this] def newBuilder = new immutable.VectorBuilder[E]
    
    override def toString = mkString("[", " ~ ", "]")
  }
  object Tracer {
    implicit def wrap[E](raw: TracerRepr[E]) = new Tracer(raw)
    implicit def unwrap[E](t: Tracer[E]): TracerRepr[E] = t.self
    def apply[E](raw: TracerRepr[E] = immutable.Vector()) = wrap(raw)
    
    final class Builder[E] extends mutable.Builder[E, Tracer[E]] {
      protected val builder = new immutable.VectorBuilder[E]()
      def += (e: E) = { builder += e; this }
      def clear = { builder; this }
      def result = new Tracer[E](builder.result)
    }
  }
  
  type BBTracer = Tracer[BB]
  final case class BBEffect(static_env_out: StaticEnv,
                            tracef_out: TraceFrame,
                            heap_out: HeapEnv,
                            uncaught_out: immutable.Set[Exceptional]) extends Immutable with NotNull
  type BBEffects = MutableConcurrentMap[BB, MutableConcurrentMap[BBTracer, BBEffect]]
  
  type FTracer = Tracer[Method]
  final case class FEffect(static_env_out: StaticEnv,
                           heap_out: HeapEnv) extends Immutable with NotNull
  type FEffects = MutableConcurrentMap[Method, MutableConcurrentMap[FTracer, FEffect]]
  
  // TODO: Let's migrate to a proper database at some point...
  final case class FIndex(static_env: StaticEnv,
                          heap: HeapEnv,
                          params: Seq[Val[SUBT[Instantiable]]]) extends Immutable
  final case class FSummary(uncaught_throws: MutableConcurrentMultiMap[EncodedTrace, Exceptional],
                            rets: MutableConcurrentMap[EncodedTrace, RetVal],
                            static_envs: MutableConcurrentMap[EncodedTrace, SFEnv],
                            heaps: MutableConcurrentMap[EncodedTrace, HeapEnv]) extends Immutable with NotNull
  type FSummaries = MutableConcurrentMap[Method, MutableConcurrentMap[FIndex, FSummary]]
                            
  /**
   * To keep "hidden" state in between two or more instruction evaluations or BBs
   * Wrt BBs, we assume that there can be no difference in eval state across two branches; e.g.
   * retval will be produced an consumed along a single sequential string of BBs, being referentially
   * transparent (as unassigned) to any branches thereafter
   */
  final case class BBEvalState(param_index: Int,
                               retval: Option[Val_],
                               error: Option[Val[Exceptional]],
                               pseudo: Option[Val_]) extends Immutable with NotNull {
    def update(param_index_ :Int      = param_index,
               retval_ :Option[Val_]             = retval,
               error_  :Option[Val[Exceptional]] = error,
               pseudo_ :Option[Val_]             = pseudo) =
      BBEvalState(param_index_, retval_, error_, pseudo_)
  }
  
  final case class BBTrace(bb: BB,
                          tracer : BBTracer,
                          _eval_state: BBEvalState,
                          _static_env: StaticEnv, _tracef: TraceFrame) extends Immutable with NotNull
  
  /**
   * 4-phase, infinitely-tiered tracing so that we can shallowly
   * follow inductive looping across both phases of all possible traces.
   * Phases are:
   * * init: if we wouldn't loop, then we don't lose precision
   * * step: if we do loop, we eventually union all the possibilities
   * * cleanup: we might infinite loop, so we go ahead and mark all changing values as unknowns
   * * done: we're done with cleanup and should never follow this trace again
   */
  final object TracePhase extends Enumeration {
    val None, Init, Step, CleanupStart, CleanupDone = Value
  }
  type TracePhase = TracePhase.Value
  def advancePhase[E](phase: TracePhase, trace: Tracer[E], fuel: Int,
                      logger: Logger = null) = {
    def log(s: String) = if(logger != null) logger(s)
    phase match {
      case TracePhase.Init =>
        log("STEPPING PHASE [0]")
        TracePhase.Step
      case TracePhase.Step =>
        val step = trace.count(_ == trace.last) 
        if(step > fuel) {
          log("START CLEANUP PHASE")
          TracePhase.CleanupStart
        }
        else {
          log("STEPPING PHASE ["+step+"]")
          TracePhase.Step
        }
      case TracePhase.CleanupStart =>
        log("END CLEANUP PHASE")
        TracePhase.CleanupDone
    }
  }
  
  final class UnimplementedOperationException(msg: String) extends Exception(msg)
  
  lazy val singleton = {assert(_singleton != null); _singleton}
  private var _singleton : CFA2Analysis[Opts] = null
  
  // Global, hackish convenience
  def log(sym:Symbol)(s:String) = singleton.opts.log(sym)(s)
}

/**
 * Assumptions:
 * * Unchecked exceptionals (Errors or RuntimeExceptions) are only caught inside of the method
 *   in which they could occur.
 *   Therefore, if they aren't caught, then they would never be and the program would terminate
 *   anyway and so there is no harm in erasing them should they not be caught.
 * * We are getting a view of an entire Android app in this compilation unit. This means that
 *   everything we see will be run in its own VM, thus we don't have to worry about certain kinds
 *   of interference, such as: static variables being assigned, etc. Therefore we can reasonably
 *   guarantee the points by which the world interfaces with the code.
 * * We don't handle multi-threading; we assume that during our trace the only way that traced
 *   vars could be changed is via our trace
 *   
 * TODO:
 * * We have analyzed all of the Android standard libraries, thus allowing us to have a
 *   pseudo-whole-program view, also including the above assumption.
 *   
 * FIXME:
 * * We don't execute the static initialization code for classes once they're "loaded" 
 */
abstract class CFA2Analysis[+O<:Opts](contexts : java.lang.Iterable[Context],
                                      protected[cfa2] final val opts:O) extends Optimizer {
  import CFA2Analysis._
  import Tracer._
  
  // Setup global environment
	{
	  System.err.println("Logging to "+opts.outPath)
    
	  if(opts.extra_classpaths != None)
	    for(url <- opts.extra_classpaths.get)
	      BuiltinAnalysisClassLoader += url
	  
	  // FIXME: HACK
	  val fileNames = dx.command.dexer.Main.args.fileNames
	  for(f <- fileNames)
	    BuiltinAnalysisClassLoader += new File(f).toURL
	  
	  // FIXME: Clever hack; actually WORKS! But redundant except for apks
	  /*import dx.cf.direct.ClassPathOpener
	  for(f <- fileNames)
	    new ClassPathOpener(f, false,
	    new ClassPathOpener.Consumer {
	      def processFileBytes(name: String, lastModified: Long, data: Array[Byte]) =
	        if(!(name endsWith ".class")) false
	        else {
	          val id = name dropRight 6
	          log('debug) ("Attempting to dynamically register "+id)
	          BuiltinAnalysisClassLoader.registerRawClass(id, data) != null
	        }
	      def onException(e: Exception) =
	        log('warn) ("Dynamically loading class for analysis failed: "+e)
	      def onProcessArchiveStart(f: File) = Unit
	    }).process()*/
	}
  
  def this(context: Context, opts: O) = this(immutable.Set(context), opts)
  
  require(contexts forall {!_.dataMap.isEmpty})
  require(_singleton == null)
  _singleton = this
  
  // HACK: close all the logs
  Runtime.getRuntime().addShutdownHook(new Thread {
    override def run() = opts.loggers map {_.finish()}
  })
  
  import opts.log
  
  /* ====== Initialization ====== */
  protected[cfa2] final val methodMap = {
    val build = {
      type Builder = mutable.MapBuilder[Method, Context, par.immutable.ParMap[Method, Context]]
      new Builder(par.immutable.ParMap()) {
        def contains(m:Method) : Boolean = elems.seq.contains(m)
      }
    }
    for(c <- contexts)
      // Sort to give some determinism
      for(raw <- c.dataMap.keySet.iterator.toSeq.sortBy(_.getName.getString)) {
        val m = Method.wrap(raw, c.dataMap.get(raw).ropMeth)
        require(!(build contains m))
        build += ((m, c))
      }
    build result
  }
  protected[cfa2] final val dataMap = {
    val build = {
      type Builder = mutable.MapBuilder[Method, Context.Data, par.immutable.ParMap[Method, Context.Data]]
      new Builder(par.immutable.ParMap())
    }
    for((m, c) <- methodMap) {
      val data = c.dataMap.get(m.raw)
      assert(data != null)
      build += ((m, data))
    }
    build result
  }
  protected[cfa2] final val classes = (for(c <- contexts) yield c.getClasses).flatten.toSeq.distinct.map (Class(_))
  protected[cfa2] final val cdis = for(c <- classes) yield c.getCDI
  /** Holds known IField slots */
  protected[cfa2] final val ifieldMap = {
    val build = {
      type Builder = mutable.MapBuilder[IFieldSpec, FieldSlot.Known,
                                        par.immutable.ParMap[IFieldSpec, FieldSlot.Known]]
      new Builder(par.immutable.ParMap())
    }
    for(cdi <- cdis)
      for(f <- cdi.getIFields())
        build += ((f.getRef, f))
    build result
  }
  /** Holds known SField slots */
  protected[cfa2] final val sfieldMap = {
    val build = {
      type Builder = mutable.MapBuilder[SFieldSpec, FieldSlot,
                                        par.immutable.ParMap[SFieldSpec, FieldSlot]]
      new Builder(par.immutable.ParMap())
    }
    for(cdi <- cdis)
      for(f <- cdi.getSFields())
        build += ((f.getRef, f))
    build result
  }
  /** Caches unknown SField slots */
  private[this] val sfieldCache = new MutableConcurrentMap[SFieldSpec, FieldSlot.Unknown]
  /*private[this] val sinitMap = {
    
  }*/
  
  /* ======== Helpers depending upon initialization ====== */
  @inline
  protected[cfa2] def liftSField(spec:SFieldSpec) : FieldSlot = 
    sfieldMap get spec match {
    case Some(slot) => slot
    case None => sfieldCache getOrElse (spec, {
        val f = new FieldSlot.Unknown(spec)
        // Cache the unknown
        sfieldCache += ((spec, f))
        f
      })
    }
  @inline
  protected[cfa2] def liftIField(spec:IFieldSpec) : FieldSlot =
    ifieldMap get spec match {
    case Some(slot) => slot
    case None => sfieldCache getOrElse (spec, {
        val f = new FieldSlot.Unknown(spec)
        // Cache the unknown
        sfieldCache += ((spec, f))
        f
      })
    }
  
  @inline
  protected[cfa2] def methodForSpec(spec: MethodSpec): Option[Method] = {
    def f(pair:(Method, Context.Data)): Some[Method] = Some(pair._1)
    (dataMap find { _._2.methRef.equals(spec) }) flatMap f
  }
  @inline
  protected[cfa2] def classForMethod(m: Method) = dataMap get m match {
    case Some(d) => Some(d.clazz)
    case None    => None
  }
  
  /* ====== Results ======= */
  /*lazy val results : Results = new Results {
    run()
  }*/
  
  /* ====== Hooks ======= */
  protected[cfa2] def instance_hooks: Iterable[InstanceHook]
  //require(!(instance_hooks exists {_ == null}))
  protected[cfa2] def clone_hooks: Iterable[CloneHook]
  //require(!(clone_hooks exists {_ == null}))
  protected[this] def umethod_hooks: Iterable[UnknownMethodHook]
  //require(!(umethod_hooks exists {_ == null}))
  protected[this] def kmethod_hooks: Iterable[KnownMethodHook]
  //require(!(kmethod_hooks exists {_ == null}))
  
  /* ====== Algorithm ===== */
  protected[this] val fsummaries = new FSummaries
  @inline
  protected[this] final def summarize(m: Method,
                                      index: FIndex,
                                      trace: EncodedTrace,
                                      ret: RetVal) : Unit = {
    prep_summarize(m, index)
    log('debug) ("Summarizing "+m+":\n"+
                 "\tIndex:  "+index+"\n"+
                 "\tRetval: "+ret)
    val rets = fsummaries(m)(index).rets
    if(!rets.contains(trace))
      rets += ((trace, ret))
    else
      rets(trace) = rets(trace) union ret
  }
  @inline
  protected[this] final def summarize(m: Method,
                                      index: FIndex,
                                      trace: EncodedTrace,
                                      uncaught_throws: Iterable[Exceptional]) : Unit = {
    prep_summarize(m, index)
    for(t <- uncaught_throws)
      fsummaries(m)(index).uncaught_throws += (trace, t)
  }
  @inline
  protected[this] final def prep_summarize(m: Method,
                                           index: FIndex) = {
    if(!fsummaries.contains(m))
      fsummaries += ((m, new MutableConcurrentMap))
    if(!fsummaries(m).contains(index)) {
      val throws = new MutableConcurrentMultiMap[EncodedTrace, Exceptional]
      val rets = new MutableConcurrentMap[EncodedTrace, RetVal]
      val static_envs = new MutableConcurrentMap[EncodedTrace, SFEnv]
      val heaps = new MutableConcurrentMap[EncodedTrace, HeapEnv]
      fsummaries(m)(index) = FSummary(throws, rets, static_envs, heaps)
    }
  }
  
  protected[this] val feffects = new FEffects
  @inline
  protected[this] final def faffect(m: Method, trace: FTracer,
                                    static_env_out: StaticEnv,
                                    heap_out: HeapEnv) = {
    if(!feffects.contains(m))
      feffects += ((m, new MutableConcurrentMap))
    val effect = FEffect(static_env_out, heap_out)
    feffects(m) += ((trace, effect)) 
  }
  
  /*
   * TODO: Not threadsafe yet! Especially not for concurrently running evals
   * E.g., we can't do STM, so we can't safely concurrently merge fsummaries
   */
  protected[this] type EvalWorklist = mutable.Map[Method, Boolean] 
  protected[this] val eval_worklist: EvalWorklist = {
    // Use an ordered map so as to give a semblance of determinism
    val build = new mutable.LinkedHashMap[Method, Boolean]()
    for(m <- methodMap.seq.keys.toSeq.sortBy(_.id.raw)
        if opts.starting_points exists {(ider:MethodIDer) => +(ider identifies m)})
      build += ((m, false))
    build
  }
  
  /*
   *  TODO: What should be the return value here?
   *  Should it be summaries?
   *  Should it be a full execution graph?
   *  Should it be some set of analysis data?
   *  
   *  FIXME: Make the <init>s go before anything else and then feed back in
   */
  def run() : Unit = {
    log('debug) ("Classes are:")
    for(c <- classes) log('debug) (c.toString)
    log('debug) ("\nMethods are:")
    for(m <- methodMap.seq.keys) log('debug) (m.toString)
    
    var continue = true
    do {
      eval_worklist find {!_._2} match {
        case None         => continue = false
        case Some((m, _)) =>
          try { eval(m, new SFEnv, new HeapEnv) }
          catch {
            case e:UnimplementedOperationException =>
              log('error) ("Unimplemented operation: "+e)
            case e:StackOverflowError  =>
              log('error) ("Stack overflow when attempting to evaluate "+m+"\n"+
                           "Consider increasing the JVM's stack size with -Xss#")
              e.printStackTrace(opts.logs('error).stream)
              if(!opts.continueOnOverflow) throw e
          }
          eval_worklist(m) = true
      } 
    } while(continue)
      
    log('debug) ("\n\n******************** Done!");
  }
  
  private[this] def eval(start: Method,
                         initial_static_env: SFEnv,
                         initial_heap: HeapEnv) = {
    log('info) ("\nEvaluating "+start)
    
    {
      // NOTE: Also takes care of any implicit this param
      val paramTs = start.getEffectiveDescriptor().getParameterTypes()
      // Params are by definition instantiable
      val params = makeUnknowns(Type(paramTs).asInstanceOf[Seq[Instantiable]])
      val summary = trace(start,
                          Tracer(),
                          new MutableConcurrentMap[FTracer, TracePhase],
                          initial_static_env,
                          initial_heap,
                          params:_*)(Val.Bottom)
    }
  
  /**
   * Here we guard against recursion and catch summaries
   */
  def trace(meth: Method,
            mtracer: FTracer,
            mtracePhases: MutableConcurrentMap[FTracer, TracePhase],
            _static_env: StaticEnv,
            _heap: HeapEnv,
            params: Val[SUBT[Instantiable]]*)
            (implicit cdeps: Val_): FSummary = {
    log('debug) ("\nTracing "+meth+" ["+mtracer.count(_ == meth)+"/"+mtracer.size+"]")
    
    var static_env = _static_env
    var heap = _heap
    
    val header = mtracer.preheader :+ meth
    val phase = mtracePhases getOrElse (header, TracePhase.Init)
    log('debug) ("MPhase is: "+phase)
    
    if(phase > TracePhase.Init &&
       mtracer.contains(meth)) {
      assert(phase < TracePhase.CleanupDone)
      phase match {
        case TracePhase.Step =>
          faffect(meth, header, static_env, heap)
        case TracePhase.CleanupStart =>
          val lastEffect = feffects(meth)(header)
          static_env = static_env ++# (static_env induceUnknowns lastEffect.static_env_out)
          heap = heap ++# (heap induceUnknowns lastEffect.heap_out)
          log('debug) ("Induced unknowns for recursion cleanup")
          faffect(meth, header, static_env, heap)
      }
    }
    
    mtracePhases(header) = advancePhase(phase, mtracer, opts.loopingFuel, log('debug))
    
    log('debug) ("Params: "+params)
    //log out meth.dump
    val findex = FIndex(static_env, heap, params)
    if(fsummaries contains meth)
      fsummaries(meth) get findex match {
        case Some(summ) =>
          log('debug) ("Found summary: "+meth+": \t"+summ)
          return summ
        case None       =>
      }
    
    // We're about to dive down a level; free up some memory
    System.gc()
    // Start method trace
    return trace_(meth, mtracer, mtracePhases, Val.Bottom, findex)
  }
  
  /**
   * It is unlikely that the same method call will be encountered twice.
   * Thus, we have no need of a memory-intensive global BBSummary set.
   * We might as well just keep it for the internal method trace (to e.g. avoid loops).
   */
  @inline
  def trace_(meth: Method,
             _mtracer: FTracer,
             mtracePhases: MutableConcurrentMap[FTracer, TracePhase],
             cdeps: Val_,
             findex: FIndex): FSummary = {
    assert(meth.arity == findex.params.length)
    
    val mtracer = _mtracer :+ meth
    log('debug) ("MTracer: "+mtracer)
    
    val bbeffects = new BBEffects()
    @inline
    def bbaffect(bb: BB, trace: BBTracer,
                 static_env_out: StaticEnv,
                 tracef_out: TraceFrame,
                 heap_out: HeapEnv,
                 uncaught_out: immutable.Set[Exceptional]) {
      if(!bbeffects.contains(bb))
        bbeffects += ((bb, new MutableConcurrentMap))
      val effect = BBEffect(static_env_out, tracef_out, heap_out, uncaught_out)
      //log('debug) ("New BBEffect for "+trace+"\n"+effect)
      bbeffects(bb) += ((trace, effect))
    }
    
    val bbtracePhases = new MutableConcurrentMap[BBTracer, TracePhase]
  
  def trace_bb(bb: BB,
               _bbtracer : BBTracer, _uncaught : immutable.Set[Exceptional],
               _eval_state: BBEvalState,
               _static_env: StaticEnv, _tracef: TraceFrame, _heap: HeapEnv)
              (implicit cdeps: Val_): /*TailRec[*/Unit/*]*/ = {
    log('debug) ("\nTracing BB "+bb+"@"+meth+" ["+_bbtracer.length+"]")
    
    val bbtracer = _bbtracer :+ bb
    log('debug) ("BBTracer: "+bbtracer)
    
    var static_env = _static_env
    log('debug) ("Static env: "+static_env)
    
    var tracef = _tracef
    log('debug) ("Trace frame: "+tracef)
    
    // Momentarily null so as to tie mutually-recursive knot
    implicit var heap: HeapEnv = null
    def updateHeap(k:Var.RawHeap_, v:Val_): Unit = {
      heap = new HeapEnv(heap +# (k, v), updateHeap)
    }
    // TODO: Ensure that we can never have the self of an env be its own type (recursive)
    heap = new HeapEnv(_heap.self, updateHeap)
    log('debug) ("Heap: "+heap)
    
    var eval_state = _eval_state
    log('debug) ("State: "+_eval_state)
    
    var uncaught = _uncaught
    
    @inline def take_param_index = {
      val i = eval_state.param_index; eval_state = eval_state.update(param_index_ = i+1)
      assert(i < findex.params.length); i
    }
    
    @inline def take_retval = {
      val r = eval_state.retval; update_retval(Val.Bottom);
      assert(r != null); r.get
    }
    @inline def update_retval(r: Val_) = eval_state = eval_state.update(retval_ = Some(r))
    
    // Can return null
    @inline def take_error = {
      val e = eval_state.error; update_error(Val.Bottom); e.get
    }
    @inline def update_error(e: Val[Exceptional]) = eval_state = eval_state.update(error_ = Some(e))
    
    @inline def take_pseudo = {
      val p = eval_state.pseudo; update_pseudo(Val.Bottom);
      assert(p != null); p.get
    }
    @inline def update_pseudo(p: Val_) = {
      assert(!(p exists {_.typ == VOID}))
      eval_state = eval_state.update(pseudo_ = Some(p))
    }
    
    @inline def lookup(reg:Var.Register_) : Val_ = tracef getOrElse (reg, {
      log('debug) ("Failed to find value for "+reg)
      Val.Unknown(reg.typ)
    })
    
    @inline def fsummarize_r(ret: RetVal) = summarize(meth, findex, bbtracer encode, ret)
    @inline def fsummarize_u(uncaught: Exceptional*) = summarize(meth, findex, bbtracer encode, uncaught)
    
    var result :Val_ = Val.Bottom
    val insns = bb.getInsns
    breakable {
    // All but the branching instruction
    for(index <- 0 until insns.size;
        ins: Instruction = insns.get(index)
        if !(ins.opcode.isInstanceOf[Branches] ||
             ins.opcode.isInstanceOf[MayEnd])) {
      log('debug) (ins toString)
      /**
       * Much of the info this is based off of was gleaned from Rops.ropFor and/or experimentation
       * TODO:
       * * If we call a function that we can't trace, it could write to any mutable static fields,
       *   so they should all be invalidated
       */
      // No-ops
      if(ins.opcode == NOP || ins.opcode == MARK_LOCAL) break
      
      val srcs = for(i <- 0 until ins.sources.size)
        yield Var.Register(ins.sources.get(i))
      
      val cst: dx.rop.cst.TypedConstant = ins match {
        case i:Instruction.Constant => i.constant.asInstanceOf[dx.rop.cst.TypedConstant]
        case _ => null
      }
      if(cst != null)
        log('debug) ("Constant: "+cst)
      
      val operands : IndexedSeq[Val_] = {
        val tmp = for(reg <- srcs) yield lookup(reg)
        if(ins.opcode.hasMixedArity &&
           srcs.length < ins.opcode.maxArity) {
          // We are shy one parameter. It must be encoded as a constant.
          assert(cst != null)
          tmp :+ liftConstant(cst)
        }
        else tmp
      }
      log('debug) ("Operands: "+operands)
      
      val sink = ins.result match {
        case null => None
        case dest => Some(Var.Register[Instantiable](dest))
      }
      
      lazy val resultT = Type(ins.operation.resultType).asInstanceOf[Instantiable]
      
      lazy val obj = ins.opcode match {
        case code:OnObject => {
          val o = operands(code.objOperand).asInstanceOf[Val[OBJECT]]
          assert(o != null); o
        }
      }
      
      ins.opcode match {
        /** General evaluation **/
        case code:Evaluable =>
          val args = operands.asInstanceOf[Seq[Val[code.ArgT]]]
          val retT = Type(ins.resultT).asInstanceOf[code.RetT]
          result = code.eval(retT, args:_*)
        
        /** General movement **/
        case code:Move =>
          result = code match {
            case MOVE               => operands(0)
            case MOVE_PARAM         => findex.params(take_param_index)
            case MOVE_EXCEPTION     => take_error match {
                // FIXME: We couldn't track the exceptional
                case Val.Top    => Val.Unknown(resultT)
                case Val.Bottom => assert(false); Val.Bottom //shouldn't happen...
                case e          => e
              }
            case MOVE_RESULT         => take_retval
            case MOVE_RESULT_PSEUDO  => take_pseudo
          }
        
        /** Getters **/
        case code:Getter =>
          val spec = cst.asInstanceOf[FieldSpec]
          result = code match {
            case GET_FIELD =>
              val slot = liftIField(spec)
              def get_field(v:VAL[OBJECT]) : Val_ = v match {
                case v: OBJECT#Instance => v~(_.apply(slot))
                case v: ?[OBJECT]       =>
                  val field = v.typ.fieldSlots get(spec) match {
                    case Some(s) => s
                    case None    => {v.typ.fieldSlots += slot; slot}
                  }
                  Val.Unknown(field.typ)
              }
              obj eval_ get_field _
            case GET_STATIC =>
              val slot = liftSField(spec)
              static_env getOrElse (slot, Val.Unknown(slot.typ))
          }
        
        /** Putters **/
        case code:Putter =>
          val spec = cst.asInstanceOf[FieldSpec]
          val put = operands(0)
          code match {
            // TODO
            case PUT_FIELD =>
              val slot = liftIField(spec)
              val new_obj = obj eval_ ((v:VAL[OBJECT]) =>
                // Unknown for Object is actually also an Instance
                v.asInstanceOf[OBJECT#Instance] ~~ (_.apply(slot, put)))
              tracef = tracef +# (srcs(1), new_obj)
            case PUT_STATIC =>
              val slot = liftSField(spec)
              static_env = static_env +# (new Var.MStaticF(spec), put)
          }
        
        /** Array ops **/
        case code:ArrayOp =>
          val varray = operands(0).asInstanceOf[Val[ARRAY]]
          val index = operands(1).asInstanceOf[Val[INT.type]]
          code match {
            case AGET => result = varray eval_ ((arr:VAL[ARRAY]) =>
              if(arr.isUnknown) Val.Unknown(resultT) // TODO
              else {
                arr.asInstanceOf[ARRAY#Instance] ~ (_.apply(index) match {
                  case None    => Val.Unknown(resultT) // TODO
                  case Some(v) => v
                })
              })
            case APUT =>
              val put = operands(0)
              // TODO
          }
        
        /** Monitor **/
        //TODO: once we acknowledge unknown monitor states, we can see if we throw an illegalmonitorstateexception
        case code:Monitor =>
          val monitored = code match {
            case MONITOR_ENTER => true
            case MONITOR_EXIT  => false
          }
          val new_obj = obj eval_ ((v:VAL[OBJECT]) =>
            // Unknown for Object is actually also an Instance
            v.asInstanceOf[OBJECT#Instance] ~~ (_.monitored_=(monitored)))
          tracef = tracef +# (srcs(0), new_obj)
        
        /** Allocation **/
        case code:Allocate =>
          val typ = Type(cst.asInstanceOf[CstType].getClassType).asInstanceOf[RefType]
          result = code match {
            case NEW_INSTANCE     => Val.Atom(typ.instance())
            case NEW_ARRAY        => {
              val atyp = typ.asInstanceOf[Instantiable#Array]
              Val.Atom(atyp.instance(Val.Bottom, ('length, operands(0))))
            }
            // TODO
            case FILLED_NEW_ARRAY => {
              log('info) ("FILLED_NEW_ARRAY\n"+
                           ins+"\n"+
                           code+"\n"+
                           cst+"\n"+
                           srcs)
              throw new UnimplementedOperationException("FILLED_NEW_ARRAY")
              Val.Unknown(resultT)
            }
          }
        
        /** Casters **/
        case code:Caster =>
          val typ = Type(cst.asInstanceOf[CstType].getClassType()).asInstanceOf[RefType]
          code match {
            case CHECK_CAST =>
              val sat = obj map {_.isValueOf(typ)}
              if(!Tri._all(sat))
                uncaught += Exceptionals.CLASS_CAST
              // TODO: How do we lift the cast knowledge?
              result =
                if(Tri._all(sat)) obj
                else Val.Unknown(typ)
            case INSTANCE_OF => result = {
              obj eval ((v: VAL[Instantiable]) => v.isValueOf(typ) match {
                case Tri.T => BOOLEAN.TRUE
                case Tri.U => BOOLEAN.unknown
                case Tri.F => BOOLEAN.FALSE
              })
            }
          }
        
        case CONST => result = liftConstant(cst)
        
        case ARRAY_LENGTH => result = {
          val varray = operands(0).asInstanceOf[Val[Instantiable#Array]]
          varray eval_ ((arr:VAL[Instantiable#Array]) =>
            if(arr.isUnknown) Val.Unknown(INT)
            else arr.asInstanceOf[Instantiable#Array#Instance]~(_.length))
        }
        
        // TODO
        case FILL_ARRAY_DATA =>
          log('info) ("FILL_ARRAY_DATA\n"+
                       ins+"\n"+
                       cst+"\n"+
                       srcs+"\n"+
                       operands)
          throw new UnimplementedOperationException("FILL_ARRAY_DATA")
        
        /** Calls **/
        case code:Call =>
          val (mdesc, known) = {
            val spec = ins.asInstanceOf[Instruction.Constant].constant.asInstanceOf[MethodSpec]
            methodForSpec(spec) match {
              case None    => (GhostMethod.wrap(spec), false)
              case Some(m) => (m, true)
            }
          }
          
          // TODO: We may be able to look up the method if it's reflected
          // FIXME: What if it returns void?
          @inline
          def call() =
            if(known) call_known()
            else      call_unknown()
          
          @inline
          def call_unknown() = {
            log('debug) ("Call unknown: "+mdesc)
            val params = mkparams(operands).toSeq
            val hookRets = (umethod_hooks map (_(mdesc, params))).flatten
            val result =
              if(hookRets isEmpty) Val.Unknown(mdesc.retT)
              else hookRets reduce (_ union _)
            eval_summary_(immutable.Set(THROWABLE),
                          immutable.Set(RetVal.Return(result)))
          }
          @inline
          def call_known() = {
            val m = mdesc.asInstanceOf[Method]
            log('debug) ("Call known: "+m)
            val params = mkparams(operands).toSeq
            val next_header = (mtracer.preheader :+ m).preheader :+ m
            val next_phase = mtracePhases get next_header
            next_phase match {
              case Some(TracePhase.CleanupDone) =>
                log('debug) ("Redundant trace; not calling: "+next_header)
                // TODO: actually refine this to use the propagated unknowns
                // though it may be the case that we eventually reach a fixed-point
                // of induced unknowns via summarization...
                call_unknown
              case _ =>
                try {
                  var sum = trace(m, mtracer, mtracePhases, static_env, heap, params:_*)
                  HOOK(kmethod_hooks, (m, params, sum), {sum = _:FSummary})
                  eval_summary(sum)
                }
                catch {
                  case e:StackOverflowError =>
            	    log('error) ("Stack overflow when attempting to call "+m+" from "+meth+"\n"+
            	                 "Consider increasing the JVM's stack size with -Xss#")
            	    if(!opts.continueOnOverflow) throw e
            	    else e.printStackTrace(opts.logs('warn).stream)
            	    call_unknown
                }
            }
          }
          
          @inline
          def mkparams(args: Iterable[Val_]) = {
            def deref[T <: Instantiable](vs: Val[T]): Val[T] = {
              val done: mutable.Set[Val[T]] = mutable.Set()
              val noneed: mutable.Set[VAL[T]] = mutable.Set()
              for(v <- vs.asSet)
                if(v.typ.isInstanceOf[OBJECT] && !v.typ.isGhost) {
                  val v_ = v.asInstanceOf[OBJECT#Instance]
                  done += (~v_).asInstanceOf[Val[T]]
                } else {
                  noneed += v
                }
              if(done isEmpty) vs
              else if(noneed isEmpty) Val.deepUnion(done)
              else Val.deepUnion(done + Val(noneed))
            }
            args map (deref(_))
          }
          
          @inline
          def eval_summary(fsumm:FSummary) = {
            val throws: Set[Exceptional] = fsumm.uncaught_throws.values.size match {
              case 0 => immutable.Set()
              case 1 => fsumm.uncaught_throws.values.head
              case _ => fsumm.uncaught_throws.values.reduce[Set[Exceptional]] (_ union _) 
            }
            eval_summary_(throws, fsumm.rets.values)
          }
          @inline
          def eval_summary_(uncaught_throws: Set[Exceptional],
                            rets: Iterable[RetVal]) {
            uncaught = uncaught union uncaught_throws
            val rs = rets reduce (_.union(_))
            if(rs.isInstanceOf[RetVal.WithReturn_])
              update_retval(rs.asInstanceOf[RetVal.WithReturn_].rv)
            if(rs.isInstanceOf[RetVal.WithThrow_]) {
              val ev = rs.asInstanceOf[RetVal.WithThrow_].ev
              update_error(ev)
              for(e <- ev.asSet)
                uncaught += e.typ
            }
          }
          
          code match {
            case INVOKE_STATIC => {
              if(Dynamic.isLiftableStaticCall(mdesc, operands:_*)) {
                log('debug) ("Lifting static call to "+mdesc)
                update_retval(Dynamic.liftStaticCall(mdesc, operands:_*))
              }
              else call()
            }
            // FIXME: INVOKE_SUPER needs to actually call the super-method
            case _ => {
              val typ = mdesc.parent.typ.asInstanceOf[OBJECT]
              val vobj = operands.head.asInstanceOf[Val[typ.type]]
              // TODO: Since the verifier has run, it must be true that all types of vobj are subtypes of typ;
              // We could perhaps harness this information...
              //assert(vobj.asSet forall (_.typ < typ == Tri.T))
              val proto = mdesc.prototype.withFirstParameter(typ.raw)
              
              // Can we dynamically lift this method?
              if(Dynamic.isLiftableCall(mdesc, vobj, operands.tail:_*)) {
                log('debug) ("Lifting instance call to "+mdesc)
                val typ_ = typ.asInstanceOf[typ.type with Dynamic[_]]
                val vargs = operands.tail.asInstanceOf[Seq[Val[`val`.Reflected[_]]]]
                @inline
                def call_dynamic(obj: typ_.Instance) = {
                  // TODO: catch exceptions and register them
                  def f(ref: typ_.Instance#Ref) = (ref\mdesc)(vargs:_*)
                  obj~f
                }
                val ret = vobj eval_ ((obj:VAL[OBJECT]) => call_dynamic(obj.asInstanceOf[typ_.Instance]))
                update_retval(ret)
              }
              else call()
            }
          }
          // End invoke-handling
      } // End instruction matching
      
      // Assign phase
      if(!ins.opcode.isInstanceOf[NoResult] && sink != None) {
        assert(result != null)
        log('debug) ("Result: "+result)
        def dependize[T <: Instantiable](v: VAL[T]): VAL[T] = v match {
          case v.typ.Unknown => v
          case v_ : T#Instance => v_.clone(cdeps)
        }
        def dependize_(v: VAL_) = dependize[Instantiable](v)
        tracef = tracef +# (sink.get, result eval dependize_)
      }
      /*
       *  Even if it's NoResult, we may propagate the result via eval_state.pseudo, i.e.
       *  for a move-result-pseudo
       */
      
      // Register uncaught exceptionals via the op itself, though not with Calls as we handle them specially
      if(!ins.opcode.isInstanceOf[Call])
        for(i <- 0 until ins.operation.exceptionTypes.size)
          uncaught += Type(ins.operation.exceptionTypes.getType(i)).asInstanceOf[Exceptional]
      
    } // End eval
    
    // Handle last instruction, which branches
    val br : Instruction = insns.getLast
    log('debug) ("Ending on "+br.toHuman)
    // We may be implicitly branching for a catch; get ready for a move-result-pseudo
    if(!br.opcode.isInstanceOf[Branches])
      update_pseudo(result)
    
    @inline def handle_end() = {
      assert(br.opcode.isInstanceOf[MayEnd])
      val retv = br.opcode match {
        case RETURN =>
          val value = br.sources.size match {
            // Void return
            case 0 => Val.Atom(VOID.singleton)
            case 1 => lookup(Var.Register(br.sources.get(0)))
          }
          RetVal.Return(value)
        case THROW =>
          // FIXME: We may end up immediately catching this, so it's inappropriate to summarize like this
          val err = lookup(Var.Register(br.sources.get(0))).asInstanceOf[Val[Exceptional]]
          update_error(err)
          RetVal.Throw(err)
      }
      fsummarize_u(uncaught.toSeq :_*)
      fsummarize_r(retv)
    }
      
    // Special hotpath for unambiguous succession; also subsumes GOTO
    if(bb.successors.size == 1) {
      // FIXME: check to see if this is correct
      if(br.opcode.isInstanceOf[MayEnd]) {
        assert(!br.opcode.isInstanceOf[End])
        handle_end()
      }
      return /*tailcall(*/trace_bb(bb.successors.head, bbtracer, uncaught, eval_state, static_env, tracef, heap)//)
    }
    else if(bb.successors.size == 0) { handle_end(); break }
    else br.opcode match {
      // TODO: No need to match on br anymore; unindent this
      // Strictly branching, with multiple possible branches
      case _ =>
        // Either we branched because we branch or because we catch
        assert((br.catchTs.size > 0) || br.opcode.isInstanceOf[Branches],
               (br.catchTs.size, br.opcode.isInstanceOf[Branches]))
        
        // FIXME: check to see if this is correct
        if(br.opcode.isInstanceOf[MayEnd]) handle_end()
               
        var static_env_ = static_env
        var tracef_ = tracef
        var heap_ = heap
        var uncaught_ = uncaught
        
        val header = bbtracer.preheader :+ bb
        val phase = bbtracePhases getOrElse (header, {
          if(bb.mayLoop) TracePhase.Init
          else TracePhase.None
        })
        log('debug) ("BBPhase is: "+phase)
        //assert(phase < TracePhase.CleanupDone)
        
        // If we're in an applicable phase, are a join point, and have looped...
        if(phase > TracePhase.Init /*&&
           bb.predecessors.size > 1 && _bbtracer.contains(bb)*/) {
          phase match {
            case TracePhase.Step =>
              bbaffect(bb, header, static_env_, tracef_, heap_, uncaught_)
            case TracePhase.CleanupStart =>
              val lastEffect = bbeffects(bb)(header)
              static_env_ = static_env ++# (static_env induceUnknowns lastEffect.static_env_out)
              tracef_ = tracef ++# (tracef induceUnknowns lastEffect.tracef_out)
              heap_ = heap ++# (heap induceUnknowns lastEffect.heap_out)
              // TODO what about uncaught?
              log('debug) ("Induced unknowns for looping cleanup")
              bbaffect(bb, header, static_env_, tracef_, heap_, uncaught_)
            case TracePhase.CleanupDone =>
              log('debug) ("Done cleaning up loop")
              break
          }
        }
        
        if(phase != TracePhase.None && phase != TracePhase.CleanupDone)
          bbtracePhases(header) = advancePhase(phase, bbtracer, opts.loopingFuel, log('debug))
        
        log('debug) ("Possible successors: "+bb.successors)
        
        // Test for successors of bb which could possibly be reached (given tracef_ and static_env_)
        /** Used to refine the environment based on the path */
        final case class BranchMonad(tracef: TraceFrame = tracef_,
                                     cdeps: Val_ = cdeps,
                                     eval_state: BBEvalState = eval_state) extends Immutable with NotNull
        val reach = mutable.Map[BB, BranchMonad]()
        
        // Do we catch a thrown exception?
        if(bb.canCatch) {
          val couldHandle = mutable.Map[BB, mutable.Set[Exceptional]]()
          if(opts.debug) {
            log('debug) ("Handlers: "+bb.handlers)
            for(handler <- bb.handlers)
              log('debug) ("Handler "+handler+" uses "+handler.first_ins)
          }
          for(i <- 0 until br.catchTs.size;
              t = Type(br.catchTs.getType(i))) {
            for(u <- uncaught)
              (t > u) match {
                case Tri.F => // Do nothing
                case tri   =>
                  val handlers = bb.handlersFor(t)
                  if(handlers.isEmpty)
                    log('debug) ("No handlers for "+t)
                  for(handler <- handlers) {
                    if(!(couldHandle contains handler))
                      couldHandle += ((handler, mutable.Set()))
                    couldHandle(handler) += u
                  }
                  // We can confirm we catch it
                  if(tri == Tri.T)
                    uncaught -= u
              }
          }
          log('debug) ("Handling configuration: "+couldHandle)
          for((handler, exnTs) <- couldHandle) {
            val vexn = Val.Unknown(exnTs)
            reach += ((handler, BranchMonad(eval_state = eval_state.update(error_ = Some(vexn)))))
          }
          // Besides handlers, we could also follow the normal flow of execution
          bb.prim_succ match {
            case Some(succ) => reach += ((succ, BranchMonad()))
            case None       =>
          }
        }
        
        // Handle branches (except for End-branches, which we did above)
        // TODO: If we've determined that a value can only go down one path of a conditional,
        // we should only propagate it down that path and not the other. Also keep in mind that
        // if we loaded e.g. a static field into a register (and haven't changed it), then our
        // conclusion should also still apply to the original static field
        if(br.opcode.isInstanceOf[Branches]) {
          val srcs = for(i <- 0 until br.sources.size)
            yield Var.Register(br.sources.get(i))
          val operands : IndexedSeq[Val_] = for(reg <- srcs) yield lookup(reg)
          br.opcode match {
            
            case code:If =>
              val typ = srcs(0).typ
              val va = operands(0)
              val vb =
                if(operands.length == 2) operands(1)
                else typ match { // We're dealing with a constant-zero comparison
                  case INT       => Val.Atom(INT.ZERO)
                  case _:RefType => Val.Atom(NULL.singleton)
                }
              val primSet1: mutable.Set[VAL_] = mutable.Set() // Along bb.prim_succ
              val primSet2: mutable.Set[VAL_] = mutable.Set()
              val altSet1: mutable.Set[VAL_] = mutable.Set() // Along bb.alt_succ
              val altSet2: mutable.Set[VAL_] = mutable.Set()
              
              for(a <- va.asSet;
                  b <- vb.asSet) {
                val result =
                if(a.isUnknown || b.isUnknown) Tri.U
                else typ match {
                  case _:RefType =>
                    // TODO: If we're EQZing and a.isNull=Tri.U, propagate that it is now Tri.T on the primary branch
                    val res = a.asInstanceOf[RefType#Instance] refEq b.asInstanceOf[RefType#Instance]
                    res ^ (code==ROpCodes.IF_NE)
                  case INT =>
                    val ia = a.asInstanceOf[INT.Instance].self
                    val ib = b.asInstanceOf[INT.Instance].self
                    Tri.lift(code match {
                      case IF_EQ => ia == ib
                      case IF_NE => ia != ib
                      case IF_LT => ia <  ib
                      case IF_LE => ia <= ib
                      case IF_GT => ia >  ib
                      case IF_GE => ia >= ib
                    })
                }
                def compileSet(taken: Boolean): Unit = {
                  val set1 = if(taken) primSet1
                             else      altSet1
                  set1 += a
                  if(operands.length == 2) {
                    val set2 = if(taken) primSet2
                               else      altSet2
                    set2 += b
                  }
                }
                result match {
                  case Tri.T => compileSet(true)
                  case Tri.F => compileSet(false)
                  case Tri.U => compileSet(true); compileSet(false)
                }
              }
              
              /* 
               * TODO: How we should actually refine this is that each VAL should be
               * uniquely tracked, and thus we can go over all such values in the statespace
               * and remove them as an impossibility if they don't exist on a branch.
               */
              def refineBranch(taken: Boolean): BranchMonad = {
                val vset1 = if(taken) primSet1
                            else      altSet1
                val vset2 = if(taken) primSet2
                            else      altSet2
                def refineRegister(kv: (Var.Register_, Val_)) : Val_ = kv match {
                  case (k, _) if k == srcs(0) =>
                    Val(vset1)
                  case (k, _) if operands.length == 2 && k == srcs(1) =>
                    Val(vset2)
                }
                // TODO: do the same for static env
                var tracef: TraceFrame = tracef_ +# (srcs(0), Val(vset1))
                var cdeps_ = Val(vset1) union cdeps
                if(operands.length == 2) {
                  tracef = tracef +# (srcs(1), Val(vset2))
                  cdeps_ = Val(vset2) union cdeps
                }
                BranchMonad(tracef, cdeps_)
              }
              
              if(!(primSet1 isEmpty)) reach += ((bb.prim_succ.get, refineBranch(true)))
              else log('debug) ("True branch not taken")
              if(!(altSet1 isEmpty)) reach += ((bb.alt_succ.get, refineBranch(false)))
              else log('debug) ("False branch not taken")
              //End IF
            
            case SWITCH => //TODO
              log('debug) ("SWITCH\n"+
                           br+"\n"+
                           srcs+"\n"+
                           operands)
              val bm = BranchMonad() //FIXME: At least need to make cdeps appropriate
              reach ++= (bb.successors map ((_, bm)))
          }
        }
        log('debug) ("Can reach: "+reach.keys+" out of "+bb.successors)
        
        if(reach.isEmpty) {
          // We've reached the end of a trace of execution
          fsummarize_u(uncaught.toSeq:_*)
          break
        }
        val branch_set =
          // FIXME: TracePhase.None shouldn't be a problem... so why do we have to check it?
          if(phase != TracePhase.None && phase < TracePhase.Step) reach.keys
          else {
          val tmp: mutable.Set[BB] = mutable.Set()
          for(s <- reach.keys) {
            val redundant =
              if(!(bbtracer contains s)) false
              // We've looped at least once
              else {
                /* Scout ahead to figure out if there is even a possible non-redundant path ahead of us.
                 * From the perspective of s, is there a trace following our successors
                 * which has not yet been finalized?
                 */
                val seen: mutable.Set[BB] = mutable.Set()
                def isWorthWhile(bb: BB, _trace: BBTracer) : Boolean = {
                  val trace = _trace :+ bb
                  val header = trace.preheader :+ bb
                  if(bb == s /*&& _trace != tracer*/)
                    bbtracePhases get header match {
                      case Some(p) => p < TracePhase.CleanupDone
                      case None    => true
                    }
                  else if(seen contains bb) false
                  else {
                    seen += bb
                    bb.successors.isEmpty || (bb.successors exists (isWorthWhile(_, trace)))
                  }
                }
                isWorthWhile(s, bbtracer)
              }
            
            if(!redundant) tmp += s
            else {
              log('debug) ("Taking "+s+" looks to be redundant; first instruction: "+s.first_ins)
              val next_header = (bbtracer :+ s).preheader :+ s
              bbtracePhases get next_header match {
                case Some(TracePhase.Step) | Some(TracePhase.CleanupStart) =>
                  tmp += s
                case _ =>
                  log('debug) ("Redundant trace; not taking: "+ next_header)
              }
            }
          }
          tmp
        }
        
        // FIXME: This is probably incorrect, and just an issue with e.g. phasing
        if(branch_set.isEmpty) {
          log('debug) ("Branching set empty, so summarizing with uncaught exceptions")
          fsummarize_u(uncaught.toSeq:_*)
        }
        else {
          log('debug) ("Will branch to: "+branch_set)
        }
        
        //val futures =
          // Use a sorted representation here for determinism
          for(s <- branch_set.toSeq.sortBy(_.label))
            /*yield future(*/trace_bb(s, bbtracer,
                                      uncaught_,
                                      reach(s).eval_state,
                                      static_env_,
                                      reach(s).tracef,
                                      heap_)(reach(s).cdeps)//)
        //awaitAll(opts.timeout*1000, futures.toSeq:_*)
    }} // End branch matching + breakable
    //done(Unit)
  } // End trace_bb
  
  trace_bb(meth.firstBlock,
           Tracer(), immutable.Set(),
           BBEvalState(0, None, None, None),
           findex.static_env, new TraceFrame, findex.heap)(cdeps)//.result
  assert((fsummaries contains meth))
  assert((fsummaries(meth) contains findex))
  
  return fsummaries(meth)(findex)
  } // End trace_
  
  } // End eval

}
