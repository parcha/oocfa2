package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._
import Type._
import env._

import dx.rop.`type`.{Type => RawType, _}

import scala.collection.{parallel => par, _}

/**
 * Represents a type which may have run-time values of it. They should be defined
 * according to the rules set out by Type.
 */
abstract class Instantiable(raw:RawType) extends Type(raw) { self =>
  
  // Open-recursion through Self at the type-level
  //type Self >: this.type <: Instantiable
  //type Self <: this.type
  final type Self = this.type
  
  /**
   * Represents a value of this type. They must be referentially distinct within the
   * abstract interpretation so that they can be individually and uniquely tracked.
   */
  sealed abstract class Value extends Immutable with NotNull with Serializable {
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
    /** extraDeps is for e.g. control dependencies */
    final def dependsUpon(vs: GenIterable[Val_], extraDeps: Val_) : Tri =
      Tri.any(vs map dependsUpon)
  }
  /**
   * This is for an entirely unknown instance of this type. Importantly, there is a similar but
   * distinct concept for RefTypes: the reference itself may be known, though the fields of the
   * instance may be unknown. One should NOT use this Unknown for that case; it is only for if the
   * reference itself is unknown.
   */
  trait Unknown extends self.Value
  final case object Unknown extends self.Value with self.Unknown {
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
  protected abstract class Instance_(protected[this] final val params: IParams,
                                     protected[this] final val deps: Val_) extends self.Value { _:Instance =>
    protected final def param[T](sym: Symbol) = iparamRegistry(sym).convert(
      params.get(sym) match {
        case Some(p) => p
        case None    => iparamRegistry(sym).genDefault
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
      val deps_ = deps union Val.Atom(this) union Val.deepUnion(extraDeps)
      var params_ = _params
      def addMissingParams(ps: IParams) = params_ ++= (for((k,v) <- ps if!(params_ contains k)) yield (k,v))
      addMissingParams(params)
      
      // FIXME: Hack
      import CFA2Analysis.singleton.clone_hooks
      HOOK(clone_hooks, (this, deps_, params_), addMissingParams)
        
      constructor(params_, deps_)
    }
    final def clone(extraDeps: Val_ *) : Instance =
      constructor(params, deps union Val.Atom(this) union Val.deepUnion(extraDeps))
                  
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
  private[this] val iparamRegistry: mutable.Map[Symbol, IParamRegistryEntry[_<:Any]] = mutable.Map()
  private final case class IParamRegistryEntry[T](
    manifest: ClassManifest[T],
    default: Option[Either[() => _<:T, T]],
    checker: T => Unit,
    converter: Any => T
  ) {
    require(manifest != null &&
            default != null)
    type T_ = T
    def check(v:Any) = if(checker!=null) checker(v.asInstanceOf[T])
    def genDefault = default match {
      case None => throw new RuntimeException
      case Some(e) => e match {
        case Left(f) => f()
        case Right(d) => d
      }
    }
    def convert(v: Any) = v match {
      case v:T => v
      case _   =>
        if(converter == null) v.asInstanceOf[T]
        else converter(v)
    }
    def clone(default: Option[Either[() => _<:T, T]]=default,
              checker: T => Unit=checker) = IParamRegistryEntry[T](manifest, default, checker, converter)
  }
  /** Subclasses should use this declarative API to denote the params that an instance requires */
  protected final def instance_param[T](sym: Symbol,
                                        checker: T=>Unit = null, converter: Any=>T=null)(implicit m: ClassManifest[T]) : Unit = {
    require(!(iparamRegistry contains sym))
    iparamRegistry(sym) = IParamRegistryEntry[T](m, None, checker, converter)
  }
  protected final def instance_param_[T](sym: Symbol, default: T,
                                         checker:T=>Unit = null, converter: Any=>T=null)(implicit m: ClassManifest[T]) = {
    require(!(iparamRegistry contains sym))
    iparamRegistry(sym) = IParamRegistryEntry[T](m, Some(Right(default)), checker, converter)
  }
  protected final def instance_param__[T](sym: Symbol, defaulter: ()=>T,
                                          checker:T=>Unit = null, converter: Any=>T=null)(implicit m: ClassManifest[T]) = {
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
          require(e.default != None)
          build += ((p, e.genDefault))
        case Some(v_) =>
          val v = e convert v_
          require(e.manifest.erasure.isInstance(v))
          e check v
          build += ((p, v))
      }
    
    //FIXME: HACK
    import CFA2Analysis.singleton.instance_hooks
    HOOK(instance_hooks, (this, deps, params), {build ++= (_:IParams)})
        
    constructor(build.result, deps)
  }
  
  // TODO: What does an Unknown mean here?
  final object Array extends RefType(self.raw getArrayType) {
    final val component_typ = self
    type Entry = Val[component_typ.type]
    private type Repr = GenMap[Int, Entry]
    
    instance_param_[Repr]('array, new par.immutable.ParHashMap())
    instance_param[Val[INT.type]]('length)
    instance_param_[Option[Entry]]('default, None)
    
    protected[this] val constructor = new Instance(_, _)
    
    protected[this] final class Instance_ (params: IParams, deps: Val_) extends super.Instance_(params, deps) { self=>
      private[this] lazy val array = param[Repr]('array)
      private[this] lazy val length = param[Val[INT.type]]('length)
      private[this] lazy val default = param[Option[Entry]]('default)
      
      protected[this] def get(i: Int): Option[Entry] = array.get(i)
      //protected[this] def length: Val[INT.type]
      //protected[this] def default: Option[Entry]
      
      protected[this] final class Ref_(env: HeapEnv) extends super.Ref_()(env) {
      def length = self.length
      def default = self.default
      
      def apply(i: Int) : Option[Entry] = get(i)
      def apply(i: VAL[INT.type]) : Option[Entry] = i match {
        case _:INT.Unknown =>
          if(array isEmpty) None
          else Some(Val.deepUnion[component_typ.type](array.seq.values))
        case i:INT.Instance =>
          get(i.self)
      }
      def apply(is: Val[INT.type]) : Option[Entry]= is match {
        case Val.Unknown(_) =>
          if(array isEmpty) None
          else Some(Val.deepUnion[component_typ.type](array.seq.values))
        case _ =>
          val poss = is map ((i: VAL[INT.type])=>apply(i))
          if(poss contains None) None
          else Some(Val.deepUnion[component_typ.type](poss map (_.get)))
      }
      
      /** Update */
      def apply(vi: Val[INT.type], entry: Entry) : Instance = vi match {
        case Val.Unknown(_) =>
          val new_array = array ++
            (for((k,v) <- array)
              yield (k,(v union entry).asInstanceOf[Entry]))
          clone(('array, new_array), ('default, Some(entry)))(vi)
        case _ =>
          val new_array = array ++
            (for(i <- vi.asSet)
              yield (i.asInstanceOf[INT.Instance].self,
                     (apply(i).get union entry).asInstanceOf[Entry]))
          clone(('array, new_array))(vi)
      }
      }
      final type Ref = Ref_
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

/** This is just a really weird type */
object RETADDR extends Instantiable(RawType.RETURN_ADDRESS) {
  val constructor = new Instance(_, _)
  protected final class Instance_ (params: IParams, deps: Val_) extends super.Instance_(params, deps)
  type Instance = Instance_
}
