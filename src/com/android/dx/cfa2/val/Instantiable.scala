package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._
import Type._
import env._
import analysis.{Analysis, CFA2Analysis}
import adt.Registrar
import dx.rop.`type`.{Type => RawType, _}
import scala.collection.{parallel => par, _}
import scala.reflect.ClassTag

/**
 * Represents a type which may have run-time values of it. They should be defined
 * according to the rules set out by Type.
 */
abstract class Instantiable(raw:RawType) extends Type(raw) with DelayedInit { self =>
  import Instantiable._
  
  // Open-recursion through Self at the type-level
  //type Self >: this.type <: Instantiable
  //type Self <: this.type
  final type Self = self.type
  val klass : Class[_]
  
  def delayedInit(body) = {
  body
  if(klass != null) {
    if(!isRegistered(klass))
      register(klass, this)
  }
  //else CFA2Analysis.singleton.opts.log('warn)(s"Could not reflect type $this")
  }
  
  /** Some "instantiables" aren't actually usable (*cough*void*cough*) **/
  val isGhost = !this.isInstanceOf[CanBeParam]
  
  /** The default value the JVM gives to uninitialized instances of this type **/
  def defaultInst: Instance
  
  /**
   * Represents a value of this type. They must be referentially distinct within the
   * abstract interpretation so that they can be individually and uniquely tracked.
   */
  //FIXME: commented out sealed because it leads to https://issues.scala-lang.org/browse/SI-6921
  /*sealed*/ abstract class Value extends Immutable with NotNull with Serializable {
    final val typ: Self = self
    final def isValueOf(t: Instantiable) = typ < t 
    val isUnknown : Boolean
    override def toString = typ+"#"+this.getClass.getSimpleName
    protected[this] val deps: Val_
    final def dependsUpon(v: VAL[_]) : Tri =
      if(v == this) Tri.T
      else deps match {
        case Val.Top    => Tri.U
        case Val.Bottom => Tri.F
        case _ =>
          val set =
            for(dep <- deps.asSet)
              yield
              if(dep.isUnknown) Tri.U
              else (dep.asInstanceOf[Instance_]) dependsUpon v
          Tri.any(set)
      }
    final def dependsUpon(vs: Val_) : Tri = vs match {
      case Val.Top    => Tri.U
      case Val.Bottom => Tri.F
      case _ =>
        val set = for(v <- vs.asSet) yield dependsUpon(v)
        Tri.any(set)
    }
    final def dependsUpon(vs: GenIterable[Val_]) : Tri =
      Tri.any(vs map dependsUpon)
      
    final def satisfies(f: Instance => Tri): Tri = this match {
      case Unknown_?() => Tri.U
      // FIXME: Why do we have to mark Instance here? Scala typechecker fail
      // HACK: Avoiding the Known_? typing issue
      case v:Instance if !v.isUnknown => f(v)
    }
    //final def satisfies(f: Instance => Boolean): Tri = this satisfies (f andThen Tri.lift)
  }
  /**
   * This is for an entirely unknown instance of this type. Importantly, there is a similar but
   * distinct concept for RefTypes: the reference itself may be known, though the fields of the
   * instance may be unknown. One should NOT use this Unknown for that case; it is only for if the
   * reference itself is unknown.
   */
  trait Unknown extends self.Value
  final object Unknown extends self.Value with self.Unknown {
    val isUnknown = true
    val deps = Val.Top
  }
  /** Constructs a fresh, unknown value */
  final def unknown : Self#Value = unknown(Val.Top)
  // By default, we take unknown to be singular with no possibility of deps
  // In subclasses we may definitively be able to identify the instance by this type alone
  // (e.g. it's a singleton), or we may be able to make use of the deps
  def unknown(deps: Val_) : Self#Value = Unknown
  
  /**
   * Instances should use methods as much as possible and have very, very few direct fields.
   * This is because instances should be easily COWable (as they are immutable) and thus should
   * minimize the internal state that they carry. An exception to this rule would be the use of
   * Scala's immutable collections, as they are efficient, shared hash tries.
   * 
   * The elaborate scheme revolving around Instances, subclasses of them, and constructing them is
   * because of Scala's lack of virtual classes.
   * Subclasses must have their Instance_ extend their superclass's and they should typically
   * directly take a generic "params" map, which they extract what they care about out of and pass on
   * to their superclass's constructor.
   * 
   * Extraction should occur via the provided "param" method and the extracted fields should be lazy vals.
   */
  // TODO: Introspect on params to identify other VALs and Vals which we depend upon
  protected[this] abstract class Instance_(protected[this] final var params: IParams,
                                           protected[this] final val deps: Val_) extends self.Value { _:Instance =>
    private var _origin: Option[self.Instance] = None
    def origin = {assert(_origin != this); _origin}
    
    protected final def param[T](sym: Symbol): T = iparamRegistry(sym).convert(
      params.get(sym) match {
        case Some(p) => p
        case None    => origin match {
          case Some(o) => o.param[T](sym) // Defer to the original
          case None    => iparamRegistry(sym).genDefault match {
            case Left(default) =>
              // Keep the generated default so we don't re-eval
              val cvt: T = iparamRegistry(sym).convert(default).asInstanceOf[T]
              params += ((sym, cvt))
              default
            case Right(default) => default
          }
        }
      }).asInstanceOf[T]
    
    final val isUnknown = deps == Val.Top || (params get 'unknown match {
      case Some(b:Boolean) => b
      case None            => false
    })
    protected final def equivalentParams(params: IParams) = this.params == params
    
    /** Can be overloaded in subclasses with cloners which take other params */
    final def clone(_params: (Symbol, Any)*)(implicit extraDeps: Val_ *) : Instance =
      clone(paramify(_params:_*), extraDeps)
    final def clone(_params: IParams, extraDeps: Seq[Val_]) : Instance = {
      val deps_ = Val(true, deps, Val.Atom(this), Val.deepUnion(extraDeps))
      var params_ = _params
      
      def addMissingParams(ps: IParams) =
        params_ ++= (for((k,v) <- ps if!(params_ contains k)) yield (k,v))
      addMissingParams(params) // TODO: is this necessary anymore?
      
      // FIXME: Hack
      import CFA2Analysis.singleton.clone_hooks
      analysis.HookedCFA2Analysis.HOOK(clone_hooks, (this, deps_, params_), addMissingParams)
        
      val clone = constructor(params_, deps_)
      clone._origin = Some(this)
      clone
    }
    final def clone(extraDeps: Val_ *) : Instance = {
      val clone = constructor(params, Val(true, deps, Val.Atom(this), Val.deepUnion(extraDeps)))
      clone._origin = Some(this)
      clone
    }
                  
    final override lazy val toString = {
      val build = new StringBuilder
      build append typ+"#{ "
      build append params
      build append "; DEPS: "+deps
      build append " }"
      build result
    }
    final override def equals(that: Any) = that match {
      case that:Instance_ => that equivalentParams this.params
      case _ => super.equals(that)
    }
    final override lazy val hashCode = params.hashCode
  }
  type Instance <: Instance_
  
  /** Should be the Instance constructor */
  protected[this] val constructor: (IParams, Val_) => self.Instance
  
  protected[this] final implicit def paramify(ps: (Symbol, Any)*) = immutable.Map(ps:_*)
  
  /** Param -> (type, default if optional, constraint checker) */
  // FIXME: Should be private[this], but isIParamRegistered triggers IllegalAccessError
  protected[this] val iparamRegistry: mutable.Map[Symbol, IParamRegistryEntry[_<:Any]] = mutable.Map()
  /** Subclasses should use this declarative API to denote the params that an instance requires */
  protected[this] final def instance_param[T]
  (sym: Symbol, checker: T=>Unit = null, converter: Any=>T=null)(implicit m: ClassManifest[T]) : Unit = {
    require(!(iparamRegistry contains sym))
    iparamRegistry(sym) = IParamRegistryEntry[T](m, None, checker, converter)
  }
  protected[this] final def instance_param_[T]
  (sym: Symbol, default: T, checker:T=>Unit = null, converter: Any=>T=null)(implicit m: ClassManifest[T]) = {
    require(!(iparamRegistry contains sym))
    iparamRegistry(sym) = IParamRegistryEntry[T](m, Some(Right(default)), checker, converter)
  }
  protected[this] final def instance_param__[T]
  (sym: Symbol, defaulter: ()=>T, checker:T=>Unit = null, converter: Any=>T=null)(implicit m: ClassManifest[T]) = {
    require(!(iparamRegistry contains sym))
    iparamRegistry(sym) = IParamRegistryEntry[T](m, Some(Left(defaulter)), checker, converter)
  }
  /*protected final def default_param[T](sym: Symbol, default: T)(implicit m: Manifest[T]) = {
    val entry = iparamRegistry(sym)
    require(entry.manifest >:> m,
            entry.default == None)
    entry.check(default)
    iparamRegistry(sym) = entry.clone(default=Some(Right(default)))
  }*/
  
  protected[this] final def isIParamRegistered(sym: Symbol) = iparamRegistry.contains(sym)
  
  /**
   * Constructs a fresh Instance
   * Args are Symbol-coded parameters that the constructor requires
   */
  final def instance(deps: Val_, params: (Symbol, Any)*): self.Instance =
    instance(deps, paramify(params:_*))
  def instance(deps: Val_ = Val.Bottom, params: IParams=paramify()): self.Instance = {
    val build = immutable.Map.newBuilder[Symbol, Any]
    build ++= params
    for((p, e) <- iparamRegistry)
      params.get(p) match {
        case None =>
          require(e.default != None, s"Default for $e of $p was None")
          build += ((p, e.genDefault_))
        case Some(v_) =>
          val v = e convert v_
          require(e.manifest.erasure.isInstance(v))
          e check v
          build += ((p, v))
      }
    
    //FIXME: HACK
    import CFA2Analysis.singleton.instance_hooks
    //HOOK(instance_hooks, (this, deps, params), {build ++= (_:IParams)})
        
    constructor(build.result, deps)
  }
  
  // TODO: What does an Unknown mean here?
  final object Array extends RefType(self.raw getArrayType) {
    final val component_typ = self
    type Entry = Val[component_typ.type]
    private type Repr = par.immutable.ParHashMap[Int, Entry]
    final val klass =
      if(component_typ.klass == null) null
      else ClassTag(component_typ.klass).wrap.runtimeClass
    
    protected override def << (t: Type) = t match {
      case t:Array        => component_typ < t.component_typ
      case _:OBJECT_.type => Tri.T
      case _              => Tri.F
    }
    protected override def >> (t: Type) = t match {
      case t:Array     => component_typ > t.component_typ
      case _:NULL.type => Tri.T
      case _           => Tri.F
    }
    
    final lazy val defaultInst = instance(Val.Bottom, ('isNull, Tri.T))
    
    type GetResult = Option[Entry]
    
    instance_param_[Repr]('array, new par.immutable.ParHashMap())
    /* length is a VAL instead of a Val because we want different-length arrays
     * put together in a Val, not held by the same Instance. This way we can e.g.
     * prune impossibilities resulting from OOB exceptions.
     */
    instance_param_[VAL[INT.type]]('length, INT.unknown)
    instance_param_[Entry]('default, Val.Atom[component_typ.type](component_typ.defaultInst))
    
    import cfa2.wrap.MethodIDer
    private[this] def _emulate_clone(r:Instance#Ref): VAL_ = r.emulate_clone
    emulatedMethod_(MethodIDer.LaxlyBySignature("clone"), _emulate_clone _)
    private[this] def _emulate_equals(r:Instance#Ref): REF_ => VAL_ =
      (that:REF_) => r.emulate_equals(that.asInstanceOf[REF[this.type]])
    emulatedMethod(MethodIDer.LaxlyBySignature("equals(Object)"),
                   EmulatedMethod.RR_1(_emulate_equals _))
    
    protected[this] val constructor = new Instance(_, _)
    
    /** See the discussion of unknowns in OBJECT **/
    override def unknown(deps: Val_) = instance(deps, ('unknown, true),
                                                      ('length, INT.unknown), // TODO: make a natural number
                                                      ('isNull, Tri.U),
                                                      ('monitored, Tri.U)).asInstanceOf[Value]
    
    protected[this] final class Instance_ (params: IParams, deps: Val_) extends super.Instance_(params, deps) { self=>
      import IndexCheck._
      private[this] lazy val array = param[Repr]('array)
      private[this] lazy val length = param[VAL[INT.type]]('length)
      private[this] lazy val default = param[Entry]('default)
      
      private[this] def checkIndex(i: Int): Tri = length match {
        case Unknown_?() => Tri.U
        case Known_INT_?(l)   => i < l.self
      }
      private[this] def checkIndex(i: VAL[INT.type]): Tri = length match {
        case Unknown_?() => Tri.U
        case Known_INT_?(l)   => i match {
          case Unknown_?() => Tri.U
          case Known_INT_?(i)   => i.self < l.self
        }
      }
      private[this] def checkIndex(i: Val[INT.type]): Tri = length match {
        case Unknown_?() => Tri.U
        case Known_INT_?(l)   =>
          if(i satisfies {_.isUnknown}) Tri.U
          else {
            val count = i.asSet count {_.asInstanceOf[INST[INT.type]].self < l.self}
            if(count == 0) Tri.F
            else if(count == i.asSet.size) Tri.T
            else Tri.U
          }
      }
      
      // TODO: Perhaps macroize?
      private[this] def wrapIndexCheck[R](i: Int, thunk: =>R): IndexCheck[R] = checkIndex(i) match {
        case Tri.T => Within(thunk)
        case Tri.U => Maybe(thunk)
        case Tri.F => Outside
      }
      private[this] def wrapIndexCheck[R](i: VAL[INT.type], thunk: =>R): IndexCheck[R] = checkIndex(i) match {
        case Tri.T => Within(thunk)
        case Tri.U => Maybe(thunk)
        case Tri.F => Outside
      }
      private[this] def wrapIndexCheck[R](i: Val[INT.type], thunk: =>R): IndexCheck[R] = checkIndex(i) match {
        case Tri.T => Within(thunk)
        case Tri.U => Maybe(thunk)
        case Tri.F => Outside
      }
      
      def NotExists =
        if(isUnknown) None
        else {
          CFA2Analysis.log('debug) (s"Reading uninitialized value from $this")
          Some(default)
        }
      
      protected[this] def get(i: Int): Option[Entry] = array.get(i)
      
      protected[this] final class Ref_(env: HeapEnv) extends super.Ref_()(env) {
      def length = self.length
      def default = self.default 
      
      def apply(i: Int) : IndexCheck[GetResult] = wrapIndexCheck(i,
        get(i) match {
          case Some(v) => Some(v)
          case None    => NotExists
        })
      def apply(i: VAL[INT.type]) : IndexCheck[GetResult] = wrapIndexCheck(i,
        i match {
        case Unknown_?() =>
          if(array isEmpty) NotExists
          else Some(Val.deepUnion[component_typ.type](array.seq.values))
        case Known_INT_?(i) =>
          get(i.self)
        })
      def apply(vi: Val[INT.type]) : IndexCheck[GetResult]= wrapIndexCheck(vi,
        vi match {
        case Val.Unknown(_) =>
          if(array isEmpty) NotExists
          else Some(Val.deepUnion[component_typ.type](array.seq.values))
        case _ =>
          val poss = vi.asSet map ((i: VAL[INT.type])=>apply(i).asInstanceOf[WithRet[GetResult]].ret)
          if(poss contains None) None
          else Some(Val.deepUnion[component_typ.type](poss map (_.get)))
        })
        
      type StorageCheck = Instantiable.StorageCheck[Instance]
      
      private[this] def checkStorage(entry: Val_): (Tri, Entry) = {
        def test(v: VAL_) = v.typ < component_typ
        val (poss, no) = entry.asSet partition {+test(_)}
        if(!no.isEmpty)
          CFA2Analysis.log('warn)(s"Found guaranteed invalid array stores for $component_typ: $no")
        if(poss.isEmpty) (Tri.F, Val.Bottom)
        else {
          val (yes, maybe) = poss partition {test(_) == Tri.T}
          if(!maybe.isEmpty) {
            import Analysis.Opts.ArrayCovarianceBehaviors._
            CFA2Analysis.singleton.opts.arrayCovarianceBehavior match {
              case Warn =>
                CFA2Analysis.log('debug)(s"Possibly invalid array stores for $component_typ: $maybe")
                (Tri.U, Val(poss).asInstanceOf[Entry])
              case Mask =>
                CFA2Analysis.log('debug)(s"Possibly invalid array stores for $component_typ: $maybe")
                (Tri.U, (Val(yes) union Val.Unknown(component_typ)).asInstanceOf[Entry])
            }
          }
          else if(no.isEmpty) (Tri.T, entry.asInstanceOf[Entry])
          else (Tri.U, Val(poss).asInstanceOf[Entry])
        }
      }
      
      def wrapStorageCheck(entry: Val_, thunk: Entry => Instance): StorageCheck = {
        import StorageCheck._
        checkStorage(entry) match {
        case (Tri.T, entry) => OK(thunk(entry))
        case (Tri.U, entry) => Maybe(thunk(entry))
        case (Tri.F, _)     => Fail
        }
      }
      
      /** Update */
      // FIXME: type of entry should be ENTRY, but everything gets really complicated...
      def apply(vi: Val[INT.type], entry: Val_): IndexCheck[StorageCheck] =
        wrapIndexCheck(vi, wrapStorageCheck(entry, {entry:Entry =>
        vi match {
        case Val.Unknown(_) =>
          val new_array: Repr = array ++
            (for((k,v) <- array)
              yield (k,(v union entry).asInstanceOf[Entry]))
          // Merge a new default, as we could've assigned anywhere
          clone(('array, new_array), ('default, default union entry))(vi)
        case _ =>
          // Only handle possibly valid indices
          val is = vi.asSet filterNot {checkIndex(_) == Tri.F}
          def validate(ret: GetResult): Entry = ret match {
            case None    => Val.Unknown(component_typ).asInstanceOf[Entry]
            case Some(v) => v
          }
          type Ret = IndexCheck.WithRet[GetResult]
          val new_entries =
            for(i_ <- is;
                i = i_.asInstanceOf[INST[INT.type]].self)
              yield (i,
                  // FIXME: May need to change if we eventually make use of programmer-defined default entries
                  if(array contains i)
                     (validate(apply(i).asInstanceOf[Ret].ret) union entry).asInstanceOf[Entry]
                  else entry)
          val new_array: Repr = array ++ new_entries
          clone(('array, new_array))(vi)
        }}))
      }
      type Ref = Ref_
      val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  final type Array = Array.type 
  
  // TODO: Make an additional Array repr that uses a lifted array
  /*final object Array extends Array {
    
  }
  // TODO: Make reflected (use arrayManifest)
  final object LiftedArray extends Array {
    assert(component_typ.isInstanceOf[Reflected[_]])
  }*/
}
object Instantiable extends Registrar[Class[_], Instantiable] {
  import registry._
  private def register(c: Class[_], t: Instantiable) = registry register (c, t)
  def typeForClass(c: Class[_]): Option[Instantiable] = registry registered c
  def isRegistered(c: Class[_]) = registry isRegistered c
  
  // FIXME: see iparamregistry
  protected case class IParamRegistryEntry[T](
    manifest: ClassManifest[T],
    default: Option[Either[() => _<:T, T]],
    checker: T => Unit,
    converter: Any => T
  ) {
    require(manifest != null &&
            default != null)
    type T_ = T
    def check(v:Any) = if(checker!=null) checker(v.asInstanceOf[T])
    // Left means we generated, right means we didn't
    def genDefault: Either[T, T] = default match {
      case None => throw new InternalError
      case Some(e) => e match {
        case Left(f) => Left(f())
        case Right(d) => Right(d)
      }
    }
    // For when you don't care either way
    def genDefault_ : T = genDefault match {
      case Left(l)  => l
      case Right(r) => r
    }
    def convert(v: Any) =
      if(converter == null) v.asInstanceOf[T]
      else converter(v)
    def clone(default: Option[Either[() => _<:T, T]]=default,
              checker: T => Unit=checker) = IParamRegistryEntry[T](manifest, default, checker, converter)
  }
  
  sealed abstract class IndexCheck[+R]
  object IndexCheck {
    sealed abstract class WithRet[R](val ret:R) extends IndexCheck[R]
    final case class Within[R](override val ret:R) extends WithRet[R](ret)
    final case class Maybe[R](override val ret:R) extends WithRet[R](ret)
    final case object Outside extends IndexCheck[Nothing]
  }
  
  sealed abstract class StorageCheck[+R]
  object StorageCheck {
    sealed abstract class WithRet[R](val ret: R) extends StorageCheck[R]
    final case class OK[R](override val ret:R) extends WithRet[R](ret)
    final case class Maybe[R](override val ret:R) extends WithRet[R](ret)
    final case object Fail extends StorageCheck
  }
}

/** This is just a really weird type */
object RETADDR extends Instantiable(RawType.RETURN_ADDRESS) {
  val klass = null
  // Actually invalid
  lazy val defaultInst = throw new UnsupportedOperationException
  val constructor = new Instance(_, _)
  protected final class Instance_ (params: IParams, deps: Val_) extends super.Instance_(params, deps)
  type Instance = Instance_
}
