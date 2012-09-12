package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import env._
import `var`._

import scala.collection._

abstract class RefType protected[`val`] (raw:RawType) extends Instantiable(raw) with Type.CanBeParam {
  import RefType._
  require(raw isReference)
  
  instance_param_[Tri]('isNull, Tri.F)
  instance_param__[Long]('heapToken, ()=>nextHeapToken)
  
  abstract class Instance_(params: IParams, deps:Val_)
  extends super.Instance_(params, deps) { self :Instance =>
    final lazy val isNull = param[Tri]('isNull)
    /** Token for keeping track of heap reference */
    final lazy val heapToken = param[Long]('heapToken)
    
    /** Tests for lifted referential equality */
    final def refEq(that: RefType#Instance) : Tri =
      if((this.isNull & that.isNull)==Tri.T) true
      else if((this.isNull ^ that.isNull)==Tri.T) false
      else if(!this.isUnknown && !that.isUnknown) heapToken == that.heapToken
      else Tri.U
    
    /**
     * This encapsulates all access to the instance via a reference hook so that
     * e.g. a newer version of this instance can be found in the environment.
     * Subclasses must ensure that all externally-accessible or mutating functionality
     * first goes through a reference. Thus, from the outside, instances deriving from
     * RefType seem to be references.
     * 
     * Instances of Ref_ should feature no initialization, as they are created VERY
     * often. Therefore, one should only use defs, storing more permanent vals in self.
     */
    protected[this] class Ref_(protected[this] implicit val env: HeapEnv)
    extends Var.RawHeap[typ.type](self.heapToken) with Immutable /*with NotNull*/ with Serializable { _:Ref =>
      // Forwarded methods from Instantiable.Instance
      @inline
      protected[this] final def clone(_params: (Symbol, Any)*)(extraDeps: Val_ *) : Instance =
        clone(paramify(_params:_*), extraDeps)
      @inline
      protected[this] final def clone(_params: IParams, extraDeps: Seq[Val_]) : Instance = {
        val clone = self.clone(_params, extraDeps)
        //Ultimate hook for any kind of "updating"
        env.update(clone.rawRef, Val.Atom(clone))
        clone
      }
    }
    type Ref <: Ref_
    /** Ref constructor; subclasses should specify their actual, final Ref */
    protected[this] val ref : HeapEnv => Ref with NotNull
    // TODO: HACK
    protected val rawRef = new Var.RawHeap(heapToken){}//{ type Source = Null; val src = null }
    /** "Dereference"; gets most up-to-date version(s) */
    final def unary_~(implicit env: HeapEnv) : Val[typ.type] = env.get(rawRef) match {
      case None      => Val.Atom(self).asInstanceOf[Val[typ.type]]
      case Some(refs) => refs.asInstanceOf[Val[typ.type]]
    }
    /** "Dereference-and-apply" */
    final def ~[T <: Instantiable](f: Instance#Ref => Val[T])(implicit env: HeapEnv) : Val[T] = env.get(rawRef) match {
      case None       => f(ref(env))
      case Some(vref) => vref.asInstanceOf[Val[typ.type]] eval_
          ((v: VAL[Instantiable]) => v.asInstanceOf[Instance] ~ f)
    }
    /** "Dereference, apply, and collect" */
    final def ~~[T <: Instantiable](f: Instance#Ref => VAL[T])(implicit env: HeapEnv) : Val[T] = env.get(rawRef) match {
      case None       => Val.Atom(f(ref(env)))
      case Some(vref) => vref.asInstanceOf[Val[typ.type]] eval_
          ((v: VAL[Instantiable]) => v.asInstanceOf[Instance] ~~ f)
    }
    /** "Dereference, apply, and custom-collect" */
    final def ~~~[T](f: Instance#Ref => T)(implicit env: HeapEnv) : GenSet[T] = env.get(rawRef) match {
      case None       => immutable.Set(f(ref(env)))
      case Some(vref) => vref.asInstanceOf[Val[typ.type]].asSet flatMap
          ((v: VAL[Instantiable]) => v.asInstanceOf[Instance] ~~~ f)
    }
    /** Assert that this is the newest value, so just dummy-reference it */
    final def unary_! = ref(null)
  }
  type Instance <: Instance_
}
object RefType {
  import java.util.concurrent.atomic.AtomicLong
  private val heapTokenCounter = new AtomicLong
  def nextHeapToken = heapTokenCounter.getAndIncrement()
}

/*
 *  FIXME: Completely unsure how to handle this
 *  * What should singleton be?
 *  * Should this extend OBJECT? If so, how are e.g. fields dealt with?
 */
case object NULL extends RefType(RawType.KNOWN_NULL) with Singleton {
  /*  NULL subtypes all RefTypes
   *  Additionally, we don't have an unknown case here because
   *  the only unknown types we could encounter would have to be
   *  non-primitives, and hence RefTypes
   */
  protected override def << (t: Type) = super.<<(t) | t.isInstanceOf[RefType]
  // TODO: Should this be Tri.U instead? Does JVM have nulls which can supertype?
  protected override def >> (t: Type) = super.>>(t) | false
  
  val singleton = Instance_
  object Instance_ extends super.Instance_(paramify(('isNull, Tri.T),
                                                    ('heapToken, 0L)),
                                           // TODO: Should this be Bottom?
                                           Val.Bottom) { self=>
    class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_()(env)
    type Ref = Ref_
    protected[this] val ref = new Ref(_)
  }
  type Instance = Instance_.type
}

/**
 * OBJECT is the top of the RefTypes. It is particularly special in that it is
 * _directly_ instantiable, as opposed to having a companion object. This is so that
 * when its klass is directly inspected, it resolves to RefType, thus making NULL
 * artificially subtype all the ref types.
 */
abstract class OBJECT(raw:RawType) extends RefType(raw) with Type.NonFinal {
  //lazy val Interfaces = 
  
  final val className = descriptorMatch.group("classname").replace('/', '.')
  final val klass = BuiltinAnalysisClassLoader.reflectClass(className) match {
    case None    => null
    case Some(c) => c
  }
  if(klass != null) {
    OBJECT.classRegistry += ((klass, this))
  }
  
  protected override def >> (t: Type) = super.>>(t) | {
    if(!t.isInstanceOf[OBJECT]) Tri.F
    else {
      val t_ = t.asInstanceOf[OBJECT]
      if(this.klass == null || t_.klass == null) Tri.U
      else this.klass isAssignableFrom t_.klass
    }}
  
  protected override def << (t: Type) = super.<<(t) | {
    if(!t.isInstanceOf[OBJECT]) Tri.F
    else {
      val t_ = t.asInstanceOf[OBJECT]
      if(this.klass == null || t_.klass == null) Tri.U
      else t_.klass isAssignableFrom this.klass
    }}
  
  /*
   *  Declarative API for subclasses to define attributes.
   *  
   *  If any Instance attr sets are overridden, the overrider should set its parent
   *  to the super-set. Using null indicates that there are no attrs.
   *  
   *  All attr sets are mutable because we may build up our knowledge of the attributes
   *  of a class given the instructions we've seen relating to it (and knowing that the
   *  bytecode had been verified so the instructions must be valid).
   */
  val fieldSlots : FieldSlotSet = new FieldSlotSet(null)
  // TODO: Unused atm
  /*val methodSlots : LinkSet[IMethodSpec, mutable.Set[IMethodSpec]] = null
  
  final val _methods = mutable.Set[SMethodSpec]()
  protected final def _method(spec:SMethodSpec) : Unit =
  {assert(!(_methods contains spec)); _methods += spec}*/
  
  /** Catches an incoming slot, registers it if needed, and creates a default for it */
  protected sealed abstract class FieldBacking extends immutable.Map[FieldSlot, VAL_] {
    def get(slot:FieldSlot): Some[VAL_] = {
      if(!(fieldSlots contains slot)) {
        // FIXME: Apparently raw may have odd prefixes...
        /*require {
          (slot.spec.getDefiningClass.getClassType == raw)
        }*/
        fieldSlots += slot
      }
      val v = slot.typ.unknown
      Some(v)
    }
    override def contains(slot:FieldSlot) = fieldSlots contains slot
    val iterator = Iterator.empty
    def + [B >: VAL_] (kv: (FieldSlot, B)) = throw new UnsupportedOperationException
    def -(key:FieldSlot) = throw new UnsupportedOperationException
  }
  /** Catches only unknown slots */
  protected final object fieldDefaults extends FieldBacking {
    override def get(slot:FieldSlot) = fieldDefaults_.get(slot) match {
      case s:Some[VAL_] => s
      case None         => super.get(slot)
    }
  }
  // Subclass-added defaults
  protected val fieldDefaults_ : mutable.Map[FieldSlot, VAL_] = mutable.Map()
  require {
    (fieldDefaults_ forall { fieldSlots contains _._1 })
  }
  
  protected final object fieldUnknowns extends FieldBacking
  
  /**
   * The reference itself is unknown, but in order to ease usage, we imply that the
   * non-emulated fields are unknown. Notably, if one clones this instance, it
   * will result in a value that is NOT unknown (because we've started tracking it).
   */
  // TODO: What do we do about the heapToken? We could alias with existing ones...
  final override def unknown(deps: Val_) = constructor(paramify(('unknown, true),
                                                                ('fieldBacking, fieldUnknowns),
                                                                ('isNull, Tri.U),
                                                                ('monitored, Tri.U)), deps)
  
  instance_param_[FieldMap]('fields,
                            new FieldMap(),
                            checker=
                              (fields: FieldMap)=>assert((fields.keySet diff fieldSlots).isEmpty),
                            converter=
                              (v:Any)=>FieldMap.wrap(v.asInstanceOf[FieldMap.M]))
  instance_param_[FieldBacking]('fieldBacking, fieldDefaults)
  instance_param_[Tri]('monitored, Tri.F)
  
  protected[this] abstract class Instance_ private[`val`] (params: IParams, deps:Val_)
  extends super.Instance_(params, deps) { self:Instance =>
    private[this] final lazy val fields = param[FieldMap]('fields)
    private[this] final lazy val fieldBacking = param[FieldBacking]('fieldBacking)
    private[this] final lazy val monitored = param[Tri]('monitored)
    
    private[this] final lazy val ifenv : IFEnv = {
      val build = IFEnv.Builder()
      for((slot,v) <- fields)
        // TODO: Not always mutable fields...
        build += ((new Var.MInstanceF(self, slot.spec), v))
      build result
    }
    
    protected[this] type OBJECT_Ref_ = Ref_
    protected[this] class Ref_(env: HeapEnv) extends super.Ref_()(env) { _:Ref=>
    final def fields = self.fields
    final def fieldBacking = self.fieldBacking
    final def monitored = self.monitored
    final def ifenv = self.ifenv
    
    final def clone(newFields: FieldMap) : Instance =
      clone(('fields, fields ++# newFields))()
    
    /* Setting of emulated fields */
    final def monitored_=(b:Boolean) = clone(('monitored, Tri.lift(b)))()
    final def monitored_=(vb:VAL[BOOLEAN.type]) = {
      val b =
        if(vb.isUnknown) Tri.U
        else if(vb.asInstanceOf[BOOLEAN.Instance].self)
          Tri.T
        else Tri.F
      clone(('monitored, b))(Val.Atom(vb))
    }
    
    /** Invocation of a method */
    //final def \ ()
    
    /** Indexing of a field */
    final def apply(f:IFieldSpec) : Val_ = apply(fieldSlots getOrRegister f)
    final def apply(f:String): Val_  = apply(fieldSlots(f))
    final def apply(f:FieldSlot): Val_ = fields.get(f) match {
      case Some(v) => v
      case None    => Val.Atom(fieldBacking(f))
    }
    /** Setting of a field; returns a copy of this instance with the different field value */
    final def apply(f:IFieldSpec, v:Val_) : Instance = apply(fieldSlots getOrRegister f, v)
    final def apply(f:FieldSlot, v:Val_) : Instance = {
      super.clone(('fields, fields +# (f, v)))()
    }
    }
    type Ref <: Ref_
  }
  type Instance <: Instance_
}
object OBJECT {
  private val classRegistry = new MutableConcurrentMap[Class[_], Object]
  def typeForClass(c: Class[_]): Option[Object] = classRegistry get c
}
