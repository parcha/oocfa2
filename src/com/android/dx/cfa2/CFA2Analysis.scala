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
import actors.Actor.actor
import scala.actors.Futures._
import scala.annotation.unchecked.uncheckedStable

object CFA2Analysis {
  
  class Opts protected[CFA2Analysis] () extends Immutable {
    val single_threaded: Boolean = false
    val debug: Boolean = true
    // In seconds
    val timeout: Long = 60
    val log = {
      def mk(sym: Symbol, primary: Logger) = {
        val toFile = new FileLogger(sym.name+".log")
        (sym, new DualLogger(primary, toFile))
      }
      val logs = immutable.Map(
          mk('out, new PrintLogger(System.out)),
          mk('info, new PrintLogger(System.err)),
          mk('warn, new PrintLogger(System.err)),
          mk('debug, new ConditionalLogger(debug, new PrintLogger(System.out))),
          mk('error, new PrintLogger(System.err)))
      new Opts.Loggers(logs)
    }
    
    // Hooks
    val instance_hooks: Iterable[InstanceHook] = immutable.Seq()
    val clone_hooks: Iterable[CloneHook] = immutable.Seq()
    val umethod_hooks: Iterable[UnknownMethodHook] = immutable.Seq()
    val kmethod_hooks: Iterable[KnownMethodHook] = immutable.Seq()
  }
  object Opts {
    final class Loggers(logs: Map[Symbol, Logger]) {
      def apply(l: Symbol) = logs(l)
    }
  }
  
  lazy val defaultOpts = new Opts
  
  final case class Results() extends Immutable
  
  type EncodedTrace = scala.Array[Int]
  type TracerRepr = immutable.Vector[BB]
  final class Tracer(val self: TracerRepr)
  extends SeqProxyLike[BB, TracerRepr] with Immutable with NotNull with Serializable {
    def dominates(t:Tracer) : Boolean =
      if(t.length > length) false
      else this endsWith(t)
    
    /** Trace from after last occurrence of bb */
    def header(bb:BB) = {
      require(contains(bb))
      slice(lastIndexOf(bb), length)
    }
    lazy val preheader = slice(lastIndexOf(last, length-2), length-1)
    
    def encode : EncodedTrace = {
      val encoded = new mutable.ArrayBuilder.ofInt()
      for(bb <- this)
        encoded += bb.hashCode
      encoded result
    }
    // For SeqProxyLike
    def seq = self
    protected[this] def newBuilder = new immutable.VectorBuilder[BB]
  }
  object Tracer {
    implicit def wrap(raw: TracerRepr) = new Tracer(raw)
    implicit def unwrap(t: Tracer): TracerRepr = t.self
    def apply(raw: TracerRepr = immutable.Vector()) = wrap(raw)
    
    final class Builder extends mutable.Builder[BB, Tracer] {
      protected val builder = new immutable.VectorBuilder[BB]()
      def += (bb: BB) = { builder += bb; this }
      def clear = { builder; this }
      def result = new Tracer(builder.result)
    }
  }
  
  type BBIndex = Tracer
  final case class BBSummary(static_env_out: StaticEnv,
                             tracef_out: TraceFrame,
                             heap_out: HeapEnv.M,
                             uncaught_out: immutable.Set[Exceptional]) extends Immutable with NotNull
  type BBSummaries = MutableConcurrentMap[BB, MutableConcurrentMap[BBIndex, BBSummary]]
  
  final case class FIndex(static_env: StaticEnv,
                          params: Seq[Val[SUBT[Instantiable]]]) extends Immutable
  final case class FSummary(uncaught_throws: MutableConcurrentMultiMap[EncodedTrace, Exceptional],
                            rets: MutableConcurrentMap[EncodedTrace, RetVal]) extends Immutable with NotNull
  type FSummaries = MutableConcurrentMap[Method, MutableConcurrentMap[FIndex, FSummary]]
                            
  //final case class TracedExceptional(v: VAL[Exceptional]) extends Exception
  
  /**
   * To keep "hidden" state in between two or more instruction evaluations or BBs
   * Wrt BBs, we assume that there can be no difference in eval state across two branches; e.g.
   * retval will be produced an consumed along a single sequential string of BBs, being referentially
   * transparent (as unassigned) to any branches thereafter
   */
  final case class BBEvalState(param_index: Int,
                               retval: Val_,
                               error: Val[Exceptional],
                               pseudo: Val_) extends Immutable with NotNull {
    def update(param_index_ :Int      = param_index,
               retval_ :Val_          = retval,
               error_ :Val[Exceptional] = error,
               pseudo_ :Val_          = pseudo) =
      BBEvalState(param_index_, retval_, error_, pseudo_)
  }
  
  final case class BBTrace(bb: BB,
                          tracer : Tracer,
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
  
  def run(contexts: java.lang.Iterable[Context], opts: Opts) = {
    (new CFA2Analysis(contexts, opts)).run
  }
  
  lazy val singleton = {assert(_singleton != null); _singleton}
  private var _singleton : CFA2Analysis = null
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
 */
final class CFA2Analysis(contexts : java.lang.Iterable[Context], private[cfa2] val opts:Opts) extends Optimizer {
  import CFA2Analysis._
  import Tracer._
  
  def this(context: Context, opts: Opts) = this(immutable.Set(context), opts)
  
  require(contexts forall {!_.dataMap.isEmpty})
  require(_singleton == null)
  _singleton = this
  
  import opts.log
  
  /* ====== Initialization ====== */
  private[cfa2] val methodMap = {
    val build = {
      type Builder = mutable.MapBuilder[Method, Context, par.immutable.ParMap[Method, Context]]
      new Builder(par.immutable.ParMap()) {
        def contains(m:Method) : Boolean = elems.seq.contains(m)
      }
    }
    for(c <- contexts)
      for(raw <- c.dataMap.keySet.iterator) {
        val m = Method.wrap(raw, c.dataMap.get(raw).ropMeth)
        require(!(build contains m))
        build += ((m, c))
      }
    build result
  }
  private[cfa2] val dataMap = {
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
  private[cfa2] val classes = (for(c <- contexts) yield c.getClasses).flatten.toSeq.distinct.map (Class(_))
  private[cfa2] val cdis = for(c <- classes) yield c.getCDI
  /** Holds known IField slots */
  private[cfa2] val ifieldMap = {
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
  private[cfa2] val sfieldMap = {
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
  private[cfa2] def liftSField(spec:SFieldSpec) : FieldSlot = 
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
  private[cfa2] def liftIField(spec:IFieldSpec) : FieldSlot =
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
  private[cfa2] def methodForSpec(spec: MethodSpec) = {
    def f(pair:(Method, Context.Data)) = Some(pair._1)
    (dataMap find { _._2.methRef.equals(spec) }).flatMap(f)
  }
  @inline
  private[cfa2] def classForMethod(m: Method) = dataMap get m match {
    case Some(d) => Some(d.clazz)
    case None    => None
  }
  
  /* ====== Results ======= */
  /*lazy val results : Results = new Results {
    run()
  }*/
  
  /* ====== Algorithm ===== */
  private[this] val fsummaries = new FSummaries
  private[this] def summarize(m: Method,
                              index: FIndex,
                              trace: EncodedTrace,
                              ret: RetVal) : Unit = {
    prep_summarize(m, index)
    log('debug) ("Summarizing "+(m, index, trace, ret))
    val rets = fsummaries(m)(index).rets
    if(!rets.contains(trace))
      rets += ((trace, ret))
    else
      rets(trace) = rets(trace) union ret
  }
  private[this] def summarize(m: Method,
                              index: FIndex,
                              trace: EncodedTrace,
                              uncaught_throws: Iterable[Exceptional]) : Unit = {
    prep_summarize(m, index)
    for(t <- uncaught_throws)
      fsummaries(m)(index).uncaught_throws += (trace, t)
  }
  @inline
  private[this] def prep_summarize(m: Method,
                                   index: FIndex) = {
    if(!fsummaries.contains(m))
      fsummaries += ((m, new MutableConcurrentMap))
    if(!fsummaries(m).contains(index)) {
      val rets = new MutableConcurrentMap[EncodedTrace, RetVal]
      val throws = new MutableConcurrentMultiMap[EncodedTrace, Exceptional]
      fsummaries(m)(index) = FSummary(throws, rets)
    }
    index
  }
  
  /*
   *  TODO: What should be the return value here?
   *  Should it be summaries?
   *  Should it be a full execution graph?
   *  Should it be some set of analysis data?  
   */
  def run() : Unit = {
    log('debug) ("Classes are:")
    for(c <- classes) log('debug) (c.toString)
    log('debug) ("\nMethods are:")
    for(m <- methodMap.seq.keys) log('debug) (m.toString)
    
    def isAccessible(meth: Method) = {
      def hasFinalClass = classForMethod(meth) match {
        case Some(c) => Class(c) is Properties.Final
        case None    => false // Conservative assumption
      }
      (meth is Properties.Public) ||
      ((meth is Properties.Protected) && !hasFinalClass)
    }
    
    /*
     * Continue on to tracing starting with all the other externally accessible methods
     * that haven't yet been evaluated (they weren't reached by main)
     */
    //val futures =
    for(meth <- methodMap.seq.keys
        if /*meth != main &&*/
        isAccessible(meth))
      /*yield future(*/eval(meth)//)
    
    //awaitAll(opts.timeout*1000, futures.toSeq:_*)
  }
  
  private[this] def eval(start: Method) = {
    log('info) ("\nEvaluating "+start)
    val static_env = new SFEnv()
    // NOTE: Also takes care of any implicit this param
    val paramTs = start.getEffectiveDescriptor().getParameterTypes()
    // Params are by definition instantiable
    val params = makeUnknowns(Type(paramTs).asInstanceOf[Seq[Instantiable]])
    trace(start, static_env, List.empty, params:_*)
  }
  
  private[this] def trace(meth: Method,
                          static_env: StaticEnv,
                          recursiveStack: List[FIndex],
                          params: Val[SUBT[Instantiable]]*) : FSummary = {
    log('debug) ("\nTracing "+meth+" ["+recursiveStack.size+"]")
    
    log('debug) ("Params: "+params)
    //log out meth.dump
    val findex = FIndex(static_env, params)
    if(fsummaries contains meth)
      fsummaries(meth) get findex match {
        case Some(summs) =>
          log('debug) ("Found summary: "+meth+": \t"+summs)
          return summs
        case None        =>
      }
    
    // We're about to dive down a level; free up some memory
    System.gc()
    // Start method trace
    return mtrace(meth, findex, recursiveStack)
  }
  
  /**
   * It is unlikely that the same method call will be encountered twice.
   * Thus, we have no need of a memory-intensive global BBSummary set.
   * We might as well just keep it for the internal method trace (to e.g. avoid loops).
   */
  @inline
  private[this] def mtrace(meth: Method, findex: FIndex, recursiveStack: List[FIndex]) : FSummary = {
    assert(meth.arity == findex.params.length)
    
    val bbsummaries = new BBSummaries()
    @inline
    def bbsummarize(bb: BB,
                                  trace: Tracer,
                                  static_env_out: StaticEnv,
                                  tracef_out: TraceFrame,
                                  heap_out: HeapEnv.M,
                                  uncaught_out: immutable.Set[Exceptional]) {
      if(!bbsummaries.contains(bb))
        bbsummaries += ((bb, new MutableConcurrentMap))
      val index = trace
      val summary = BBSummary(static_env_out, tracef_out, heap_out, uncaught_out)
      bbsummaries(bb) += ((index, summary))
    }
    
    val tracePhases = new MutableConcurrentMap[Tracer, TracePhase]
  
  def trace_bb(bb: BB,
               _tracer : Tracer, _uncaught : immutable.Set[Exceptional],
               _eval_state: BBEvalState,
               _static_env: StaticEnv, _tracef: TraceFrame, _heap: HeapEnv.M) : Unit = {
    log('debug) ("\nTracing BB "+bb+"@"+meth+" ["+_tracer.length+"]")
    
    val tracer = _tracer :+ bb
    log('debug) ("Tracer: "+tracer)
    
    var static_env = _static_env
    log('debug) ("Static env: "+static_env)
    
    var tracef = _tracef
    log('debug) ("Trace frame: "+tracef)
    
    // Momentarily null so as to tie mutually-recursive knot
    implicit var heap: HeapEnv = null
    def updateHeap(k:Var.RawHeap_, v:Val_): Unit = {
      heap = new HeapEnv(heap +# (k, v), updateHeap)
    }
    heap = new HeapEnv(_heap, updateHeap)
    log('debug) ("Heap: "+heap)
    
    var eval_state = _eval_state
    log('debug) ("State: "+_eval_state)
    
    var uncaught = _uncaught
    
    @inline
    def take_param_index = {
      val i = eval_state.param_index; eval_state = eval_state.update(param_index_ = i+1)
      assert(i < findex.params.length); i
    }
    @inline
    def take_retval = {
      val r = eval_state.retval; update_retval(null);
      assert(r != null); r
    }
    @inline
    def update_retval(r: Val_) = eval_state = eval_state.update(retval_ = r)
    // Can return null
    @inline
    def take_error = {
      val e = eval_state.error; update_error(null); e
    }
    @inline
    def update_error(e: Val[Exceptional]) = eval_state = eval_state.update(error_ = e)
    @inline
    def take_pseudo = {
      val p = eval_state.pseudo; update_pseudo(null);
      assert(p != null); p
    }
    @inline
    def update_pseudo(p: Val_) = eval_state = eval_state.update(pseudo_ = p)
    
    @inline
    def lookup(reg:Var.Register_) : Val_ = tracef getOrElse (reg, {
      log('debug) ("Failed to find value for "+reg)
      Val.Unknown(reg.typ)
    })
    @inline
    def fsummarize_r(ret: RetVal) = summarize(meth, findex, tracer encode, ret)
    @inline
    def fsummarize_u(uncaught: Exceptional*) = summarize(meth, findex, tracer encode, uncaught)
    
    var result :Val_ = null
    val insns = bb.getInsns
    // All but the branching instruction
    for(index <- 0 until insns.size;
        ins: Instruction = insns.get(index)
        if !ins.opcode.isInstanceOf[Branches]) {
      log('debug) (ins.toHuman)
      /*
       * Much of the info this is based off of was gleaned from Rops.ropFor and/or experimentation
       * TODO:
       * * If we call a function that we can't trace, it could write to any mutable static fields,
       *   so they should all be invalidated
       */
      // No-ops
      if(ins.opcode == NOP || ins.opcode == MARK_LOCAL) return
      
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
                case null => Val.Unknown(resultT)
                case e    => e
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
          val varray = operands(0).asInstanceOf[Val[Instantiable#Array]]
          val index = operands(1).asInstanceOf[Val[INT.type]]
          code match {
            case AGET => result = varray eval_ ((arr:VAL[Instantiable#Array]) =>
              if(arr.isUnknown) Val.Unknown(resultT) // TODO
              else {
                arr.asInstanceOf[Instantiable#Array#Instance] ~ (_.apply(index) match {
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
          tracef = tracef +# (srcs(1), new_obj)
        
        /** Allocation **/
        case code:Allocate =>
          val typ = Type(cst.asInstanceOf[CstType].getClassType).asInstanceOf[RefType]
          result = code match {
            case NEW_INSTANCE     => Val.Atom(typ.instance())
            // TODO
            case NEW_ARRAY        => {
              val atyp = typ.asInstanceOf[Instantiable#Array]
              Val.Atom(atyp.instance(Val.Bottom, ('length, operands(0))))
            }
            case FILLED_NEW_ARRAY => {
              log('info) ("FILLED_NEW_ARRAY\n"+
                           ins+"\n"+
                           code+"\n"+
                           cst+"\n"+
                           srcs)
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
                else Val.Unknown(resultT)
            case INSTANCE_OF => result = {
              obj eval ((v: VAL[Instantiable]) => v.isValueOf(typ) match {
                case Tri.T => Val.Atom(BOOLEAN.TRUE)
                case Tri.U => Val.Unknown(BOOLEAN)
                case Tri.F => Val.Atom(BOOLEAN.FALSE)
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
        
        /** Calls **/
        case code:Call =>
          val spec = ins.asInstanceOf[Instruction.Constant].constant.asInstanceOf[MethodSpec]
          
          def mkparams(args: Iterable[Val_]) = {
            def deref[T <: Instantiable](vs: Val[T]): Val[T] = {
              val done: mutable.Set[Val[T]] = mutable.Set()
              val noneed: mutable.Set[VAL[T]] = mutable.Set()
              for(v <- vs.asSet)
                if(v.typ.isInstanceOf[OBJECT]) {
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
          def eval_summary(fsumm:FSummary) =
            eval_summary_(fsumm.uncaught_throws.values.reduce[Set[Exceptional]] (_ union _),
                          fsumm.rets.values)
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
          
          @inline
          def call_unknown(retT: Instantiable) = {
            val params = mkparams(operands).toSeq
            val hookRets = (opts.umethod_hooks map (_(spec, params))).flatten
            val result =
              if(hookRets isEmpty) Val.Unknown(retT)
              else hookRets reduce (_ union _)
            eval_summary_(immutable.Set(THROWABLE),
                          immutable.Set(RetVal.Return(result)))
          }
          @inline
          def call_known(m: Method, retT: Instantiable) = {
            var static_env_ = static_env
            var recursiveStack_ : List[FIndex] = List.empty
            var params = mkparams(operands).toSeq
            
            // Guard against a recursive call
            if(m == meth)
              // Check for sentinel telling us we were cleaning up
              if(!recursiveStack.isEmpty && recursiveStack.head == null) {
                // TODO
              }
              else {
                // Have we reached a stop condition?
                if((recursiveStack contains findex) ||
                   recursiveStack.size > 100) {
                  log('debug) ("Reached stop condition for recursively calling "+meth+", having used "+findex)
                  static_env_ = static_env ++# static_env.induceUnknowns(_static_env)
                  recursiveStack_ = null +: recursiveStack
                  // TODO: params?
                }
                // We're not stopping yet; push our index onto the stack
                else {
                  log('debug) ("Recursively calling "+meth+" using "+findex)
                  recursiveStack_ = findex +: recursiveStack
                }
              }
            else
              log('debug) ("Non-recursive call from "+meth+" to "+m)
            //try {
            
            var result = trace(m, static_env_, recursiveStack_, params:_*)
            HOOK(opts.kmethod_hooks, (spec, params, result), {result = _:FSummary})
            eval_summary(result)
            /*}
            catch {
              case e:StackOverflowError =>
                log('warn) ("Stack overflow when attempting to call "+m+" from "+meth)
                call_unknown(retT)
            }*/
          }
          
          code match {
            case INVOKE_STATIC =>
              val retT = Type(spec.getPrototype.getReturnType).asInstanceOf[Instantiable]
              methodForSpec(spec) match {
                case None  => call_unknown(retT)
                case Some(m) => call_known(m, retT)
              }
            case _ => {
              val typ = Type(spec.getDefiningClass.getClassType).asInstanceOf[OBJECT]
              val vobj = operands.head.asInstanceOf[Val[typ.type]]
              // TODO: Since the verifier has run, it must be true that all types of vobj are subtypes of typ;
              // We could perhaps harness this information...
              //assert(vobj.asSet forall (_.typ < typ == Tri.T))
              val proto = spec.getPrototype.withFirstParameter(typ.raw)
              val retT = Type(proto.getReturnType).asInstanceOf[Instantiable]
              // Can we dynamically lift this method?
              if(Dynamic.isLiftableCall(spec, vobj, operands.tail:_*)) {
                log('debug) ("Lifting call to "+spec.getNat.getName.getString)
                val typ_ = typ.asInstanceOf[typ.type with Dynamic[_]]
                val vargs = operands.tail.asInstanceOf[Seq[Val[`val`.Reflected[_]]]]
                def call_dynamic(obj: typ_.Instance) = {
                  // TODO: catch exceptions and register them
                  def f(ref: typ_.Instance#Ref) = (ref\spec)(vargs:_*)
                  obj~f
                }
                val ret = vobj eval_ ((obj:VAL[OBJECT]) => call_dynamic(obj.asInstanceOf[typ_.Instance]))
                update_retval(ret)
              }
              else methodForSpec(spec) match {
                // TODO: We may be able to look up the method if it's reflected
                // FIXME: What if it returns void?
                case None    => call_unknown(retT)
                case Some(m) => call_known(m, retT)
              }
            }
          }
          // End invoke-handling
      } // End instruction matching
      
      // Assign phase
      if(!ins.opcode.isInstanceOf[NoResult] && sink != None) {
        assert(result != null)
        tracef = tracef +# (sink.get, result)
      }
      if(sink != None)
        assert(result != null)
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
    
    
    // Special hotpath for unambiguous succession; also subsumes GOTO
    if(bb.successors.size == 1) {
      // We may be implicitly branching for a catch; get ready for a move-result-pseudo
      update_pseudo(result)
      return trace_bb(bb.successors.head, tracer, uncaught, eval_state, static_env, tracef, heap)
    }
    else br.opcode match {
      // No successors
      case code:End =>
        assert(bb.successors.size == 0)
        log('debug) ("Sources: "+br.sources)
        val retv = code match {
          case RETURN =>
            val value = br.sources.size match {
              // Void return
              case 0 => Val.Atom(VOID.singleton)
              case 1 => lookup(Var.Register(br.sources.get(0)))
            }
            RetVal.Return(value)
          case THROW =>
            val err = lookup(Var.Register(br.sources.get(0))).asInstanceOf[Val[Exceptional]]
            RetVal.Throw(err)
        }
        fsummarize_u(uncaught.toSeq :_*)
        fsummarize_r(retv)
        return
        
      // Strictly branching, with multiple possible branches
      // FIXME: What do we do if there's more than one catch?
      case _ =>
        // Either we branched because we branch or because we catch
        assert((br.catchTs.size == 1) ^ br.opcode.isInstanceOf[Branches],
               (br.catchTs.size, br.opcode.isInstanceOf[Branches]))
        assert(bb.successors.size > 1)
        
        var static_env_ = static_env
        var tracef_ = tracef
        var heap_ = heap
        var uncaught_ = uncaught
        
        val header = tracer.preheader :+ bb
        var phase = tracePhases getOrElse (header, {
          if(bb.mayLoop) TracePhase.Init
          else TracePhase.None
        })
        log('debug) ("Phase is: "+phase)
        // If we're in an applicable phase, are a join point, and have looped...
        if(phase > TracePhase.Init &&
           bb.predecessors.size > 1 && _tracer.contains(bb)) {
          assert(phase < TracePhase.CleanupDone)
          phase match {
            case TracePhase.Step =>
              bbsummarize(bb, header, static_env_, tracef_, heap_, uncaught_)
            case TracePhase.CleanupStart =>
              val lastSumm = bbsummaries(bb)(header)
              static_env_ = static_env ++# (static_env induceUnknowns lastSumm.static_env_out)
              tracef_ = tracef ++# (tracef induceUnknowns lastSumm.tracef_out)
              heap_ = heap ++# (heap induceUnknowns lastSumm.heap_out)
              // TODO what about uncaught?
              log('debug) ("Induced unknowns for cleanup")
              bbsummarize(bb, header, static_env_, tracef_, heap_, uncaught_)
          }
        }
        if(phase == TracePhase.Init)
          tracePhases(header) = TracePhase.Step
        // Register final BBSummary
        /*if(phase != TracePhase.None)
          bbsummarize(bb, tracer, static_env_, tracef_, heap_, uncaught_)*/
        
        val reach = mutable.Map[BB, TraceFrame]()
        // Test for successors of bb which could possibly be reached (given tracef_ and static_env_)
        
        // Do we catch a thrown exception?
        if(bb.canCatch) {
          var catches = false
          for(i <- 0 until br.catchTs.size;
              t = Type(br.catchTs.getType(i)))
            for(u <- uncaught)
              (t > u) match {
                case Tri.F => // Do nothing
                case tri   =>
                  catches = true
                  // We can confirm we catch it
                  if(tri == Tri.T)
                    uncaught -= u
              }
          if(catches) reach += ((bb.alt_succ, tracef_))
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
              val primSet: mutable.Set[VAL_] = mutable.Set() // Along bb.prim_succ
              val altSet: mutable.Set[VAL_] = mutable.Set() // Along bb.alt_succ
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
                    Tri.liftBoolean(code match {
                      case IF_EQ => ia == ib
                      case IF_NE => ia != ib
                      case IF_LT => ia < ib
                      case IF_LE => ia <= ib
                      case IF_GT => ia > ib
                      case IF_GE => ia >= ib
                    })
                }
                def compileSet(set: mutable.Set[VAL_]) = {
                  set += a
                  if(operands.length == 2) set += b
                }
                result match {
                  case Tri.T => compileSet(primSet)
                  case Tri.F => compileSet(altSet)
                  case Tri.U => compileSet(primSet); compileSet(altSet)
                }
              }
              def refineBranch(vset: Set[VAL_]): TraceFrame = {
                // TODO: do the same for static env
                tracef_ ++ tracef_ flatMap (kv => {
                  val (k, vs) = kv
                  val inter = vs.asSet & vset
                  inter.size match {
                    /* TODO: Shouldn't it be true that if the value is entirely excluded, it isn't used on the branch?
                             There are failure cases for this, but I've yet to determine why...*/
                    case 0 | vs.size => None 
                    case _ => Some(k, Val(vs.asSet diff inter))
                  }
                })
              }
              if(!(primSet isEmpty)) reach += ((bb.prim_succ, refineBranch(primSet)))
              else log('debug) ("True branch not taken")
              if(!(altSet isEmpty)) reach += ((bb.alt_succ, refineBranch(altSet)))
              else log('debug) ("False branch not taken")
            
            case SWITCH => //TODO
              log('info) ("SWITCH\n"+
                           br+"\n"+
                           srcs+"\n"+
                           operands)
              reach ++= (bb.successors map ((_, tracef_)))
          }
        }
        log('debug) ("Can reach: "+reach+" out of "+bb.successors)
        
        if(reach.isEmpty) {
          // We've reached the end of a trace of execution
          fsummarize_u(uncaught.toSeq:_*)
          return
        }
        val branch_set =
          if(phase < TracePhase.Step) reach.keys
          else if(phase == TracePhase.CleanupDone) immutable.Set()
          else {
          val tmp: mutable.Set[BB] = mutable.Set()
          for(s <- reach.keys) {
            val redundant =
              if(!(tracer contains s)) false
              // We've looped at least once
              else {
                /* Scout ahead to figure out if there is even a possible non-redundant path ahead of us.
                 * From the perspective of s, is there a trace following our successors
                 * which has not yet been finalized?
                 */
                val seen: mutable.Set[BB] = mutable.Set()
                def isWorthWhile(bb: BB, _trace: Tracer) : Boolean = {
                  val trace = _trace :+ bb
                  val header = trace.preheader :+ bb
                  if(bb == s /*&& _trace != tracer*/)
                    tracePhases get header match {
                      case Some(p) => p < TracePhase.CleanupDone
                      case None    => true
                    }
                  else if(seen contains bb) false
                  else {
                    seen += bb
                    bb.successors.isEmpty || (bb.successors exists (isWorthWhile(_, trace)))
                  }
                }
                isWorthWhile(s, tracer)
              }
            
            if(!redundant) tmp += s
            else {
              val header = (tracer :+ s).preheader :+ s
              tracePhases.get(header) match {
                case None =>
                  log('debug) ("Redundant trace; not taking: "+ header)
                case Some(TracePhase.Step) =>
                  log('debug) ("Redundant trace; starting cleanup: "+header)
                  tracePhases(header) = TracePhase.CleanupStart
                  tmp += s
                case Some(TracePhase.CleanupStart) =>
                  log('debug) ("Ending cleanup along: "+header)
                  tracePhases(header) = TracePhase.CleanupDone
                  tmp += s
              }
            }
          }
          tmp
        }
        log('debug) ("Can branch to: "+branch_set)
        
        //val futures =
          for(s <- branch_set)
            /*yield future(*/trace_bb(s, tracer, uncaught_, eval_state, static_env_, reach(s), heap_)//)
        
        //awaitAll(opts.timeout*1000, futures.toSeq:_*)
        return
    } // End branch matching
    assert(false) // Shouldn't drop to here
  } // End trace_bb
  
  trace_bb(meth.firstBlock,
           Tracer(), immutable.Set(),
           BBEvalState(0, null, null, null),
           findex.static_env, new TraceFrame, HeapEnv.defaultM)//.result
  assert((fsummaries contains meth) && (fsummaries(meth) contains findex))
  
  return fsummaries(meth)(findex)
  } // End mtrace

}
