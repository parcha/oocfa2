package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import CFA2Analysis.singleton.opts.log
import env._
import Tri._
import parsers._
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._

/**
 * Also counts as the top type.
 * WRT subtypes:
 * * They are in all-caps if they represent a lifted type; uppercase otherwise
 *   (e.g. a kind of types)
 * * An object represents a final class
 * * A class represents an inheritable class; its constructor should be protected and if it
 *   represents a concrete class it should have a companion object extending itself, serving
 *   as its "actual" lifted Type
 * * A case object represents a singleton (and thus should inherit from Singleton)
 */
abstract case class Type private[`val`] (val raw:RawType) extends Immutable with NotNull {
  import Type._
  //if(!(this eq Type.BOTTOM)) {
    require(raw != null)
    require(!isRegistered(raw), s"Attempted to register $raw twice!")
	register(raw, this)
  //}
  
  def descriptor = raw.getDescriptor
  protected[this] val descriptorMatch = (descriptorRegex findFirstMatchIn descriptor).get
  lazy val name = raw.toHuman()
  
  private final val liftedClass = this match {
    case _:Instancing => this.getClass.getSuperclass
    case _            => this.getClass
  }
  
  /** isSupertypeOf */
  final def > (t: Type) : Tri =
    t == this | concensus(
    this >> t , t << this)
  /** Non-mutually-recursive, strictly isSupertypeOf */
  protected def >> (t: Type) : Tri = Tri.U/*
    if(liftedClass isAssignableFrom t.liftedClass)
      Tri.T
    else if(t.isInstanceOf[NotBuiltIn] || this.isInstanceOf[NotBuiltIn])
      Tri.U
    else
      Tri.F*/
  
  /** isSubtypeOf */
  final def < (t: Type) : Tri =
    t == this | concensus(
    this << t , t >> this)
  /** Non-mutually-recursive, strictly isSubtypeOf */
  protected def << (t: Type) : Tri = Tri.U/*
    if(t.liftedClass isAssignableFrom liftedClass)
      Tri.T
    else if(this.isInstanceOf[NotBuiltIn] || t.isInstanceOf[NotBuiltIn])
      Tri.U
    else
      Tri.F*/
}
object Type extends Registrar[RawType, Type] {
  import registry._
  private def register(raw: RawType, typ: Type) = registry.register(raw, typ)
  private def isRegistered(raw: RawType) = registry.isRegistered(raw)
  
  /*case object BOTTOM extends Instantiable(null) {
    override def equals(other: Any) = false
  }*/
  
  type Instancing = Type with scala.Singleton
  
  trait NonFinal extends Type
  trait Singular extends Type
  trait CanBeParam extends Type
  type Movable = Instantiable
  type Returnable = Instantiable
  
  trait Ordered extends Instantiable {
    /*implicit val lifter : scala.Ordering[Self#Instance]
    trait Instance extends super.Instance with scala.Ordered[Self#Instance] {
      final def compare(that:Self#Instance): Int = lifter.compare(this, that)
    }*/
  }
  trait Numeric extends Instantiable with Ordered {
    //implicit val lifter : scala.Numeric[Self#Instance]
  }
  trait Integral extends Numeric {
    //implicit val lifter : scala.Integral[Self#Instance]
  }
  trait Fractional extends Numeric {
    //implicit val lifter : scala.Fractional[Self#Instance]
  }
  
  trait NotBuiltIn extends OBJECT
  trait Incomplete extends NotBuiltIn
  
  def apply(raw: RawType) = intern(raw)
  def apply(raws: StdTypeList) : Seq[Type] = {
    for(i <- 0 until raws.size)
      yield apply(raws.get(i))
  }
  def apply(klass: Class[_]): Type =
    intern(RawType.internClassName(klass.getName.replaceAll("\\.","/")))
  def apply[C: ClassTag]: Type = apply(classTag[C].runtimeClass)
  /** May throw IllegalArgumentException if the class name given isn't valid **/
  def apply(type_id: TypeIDExpr): Type = intern(RawType.intern(type_id.descriptor))
  
  implicit def wrap(raw: RawType) = intern(raw)
  implicit def unwrap(t: Type) = t.raw
  
  // Fast-path intern for the common case of the type being registered
  private def intern(raw:RawType) : Type = registry registered raw match {
    case Is(t)     => t
    case Was | Not =>
      // Synchronize around the raw type in case we need to insert
      raw.synchronized { _intern(raw) }
  }
  // Slower-path when we might need to insert
  private def _intern(raw:RawType) : Type = registry registered raw match {
    case Is(t) => t
    case Was   =>
      log('warn) ("Previously registered type for "+ raw +" now unreferenced")
      registerIncomplete(raw)
    case Not   => registerIncomplete(raw)
  }
  // Not a pre-registered type; most importantly: must be a reftype
  private def registerIncomplete(raw: RawType) : RefType = {
    assert(!raw.isPrimitive)
    if(raw.isArray) registerArray(raw.getComponentType)
    else registerReflected(raw)
  }
  private def registerArray(raw: RawType) : ARRAY_ = {
    // Recurse through nested array types
    def drill(raw:RawType) : ARRAY_ = {
      val t =
        if(raw.isArray) drill(raw.getComponentType)
        else Type(raw).asInstanceOf[Instantiable]
      t.Array
    }
    drill(raw)
  }
  private def registerReflected(raw: RawType) : OBJECT with Incomplete = {
    lazy val default: OBJECT with Incomplete =
      new OBJECT(raw) with Incomplete {
        protected val constructor = new Instance(_, _)
        protected class Instance_ private[`val`] (params: IParams, deps: Val_)
        extends super.Instance_(params, deps) {
          class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
          type Ref = Ref_
          protected[this] val ref = new Ref(_)
        }
        type Instance = Instance_
      }
    // FIXME: Somehow dynamically inject dependencies here, or at least macroize built-ins
    // HACK: seek out exceptionals and register them appropriately
    BuiltinAnalysisClassLoader.reflectClass(raw) match {
      case None => default
      case Some(klass) => ClassTag(klass) match {
        case t if t <:< classTag[Throwable] =>
          new Exceptionals.THROWABLE(raw) with Incomplete {
            protected val constructor = new Instance(_, _)
            protected class Instance_ private[`val`] (params: IParams, deps: Val_)
            extends super.Instance_(params, deps) {
              class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
              type Ref = Ref_
              protected[this] val ref = new Ref(_)
            }
            type Instance = Instance_
          }
        case _ => default
      }
    }
  }
  
  // Force built-ins for eager evaluation
  val built_in = new {
  import cfa2.{`val` => t}
  val RETADDR = t.RETADDR
  // PrimitiveType
  val VOID = t.VOID
  val BOOLEAN = t.BOOLEAN
  val BYTE = t.BYTE
  val CHAR = t.CHAR
  val DOUBLE = t.DOUBLE
  val FLOAT = t.FLOAT
  val INT = t.INT
  val LONG = t.LONG
  val SHORT = t.SHORT
  // RefType
  val NULL = t.NULL
  // BoxedType
  import t.{BoxedType => bt}
  val _VOID = bt.VOID
  val _BOOLEAN = bt.BOOLEAN
  val _BYTE = bt.BYTE
  val _CHAR = bt.CHAR
  val _DOUBLE = bt.DOUBLE
  val _FLOAT = bt.FLOAT
  val _INT = bt.INT
  val _LONG = bt.LONG
  val _SHORT = bt.SHORT
  // Common object types
  val OBJECT_ = t.OBJECT_
  val CLASS = t.CLASS
  val STRING = t.STRING
  // FIXME: Reenable when the dynamics are fixed
  /*val BIG_INT = t.BIG_INT
  val BIG_DEC = t.BIG_DEC
  val LOCALE = t.LOCALE*/
  // Exceptionals
  import t.{Exceptionals => e}
  val Exceptional = new {
    val THROWABLE = e.THROWABLE
    val EXCEPTION = e.EXCEPTION
    val RUNTIME = e.RUNTIME
    val ARITHMETIC = e.ARITHMETIC
    val ARRAY_INDEX = e.ARRAY_INDEX
    val ARRAY_STORE = e.ARRAY_STORE
    val CLASS_CAST = e.CLASS_CAST
    val ILLEGAL_MONITOR = e.ILLEGAL_MONITOR
    val NEGATIVE_ARRAY_SIZE = e.NEGATIVE_ARRAY_SIZE
    val NULL_POINTER = e.NULL_POINTER
    val ERROR = e.ERROR
  }
  }
}