package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import CFA2Analysis.singleton.opts.log
import env._
import Tri._

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
    require(!typeRegistry.isRegistered(raw))
	typeRegistry.register(raw, this)
  //}
  
  def descriptor = raw.getDescriptor
  val descriptorMatch = (descriptorRegex findFirstMatchIn descriptor).get
  
  /**
   *  By the rules of Type, Scala objects represent instantiating classes,
   *  therefore the actual lifted class this Type represents is the superclass of
   *  its direct Type.
   */
  /*private final val liftedClass = this match {
    //case _:Incomplete => this.getClass.getSuperclass.getS
    case _:Instancing => this.getClass.getSuperclass
    case _            => this.getClass
  }*/
  
  /** isSupertypeOf */
  final def > (t: Type) : Tri =
    t == this | concensus(
    this >> t , t << this)
  /** Non-mutually-recursive, strictly isSupertypeOf */
  protected def >> (t: Type) : Tri = Tri.U/* =
    if(liftedClass isAssignableFrom t.liftedClass) Tri.T
    else if(t.isInstanceOf[NotBuiltIn] ||
            // If we're asking wrt a primitve type, no user type can subtype one
            (this.isInstanceOf[NotBuiltIn] && this.isInstanceOf[RefType]))
      /* TODO: Currently, since we don't parse type descriptors, if we determine it's not
       * a subtype, it may very well be because we have an incomplete hierarchy */
      Tri.U
    else Tri.F*/
  
  /** isSubtypeOf */
  final def < (t: Type) : Tri =
    t == this | concensus(
    this << t , t >> this)
  /** Non-mutually-recursive, strictly isSubtypeOf */
  protected def << (t: Type) : Tri = Tri.U/*=
    if(t.liftedClass isAssignableFrom liftedClass) Tri.T
    else if(t.isInstanceOf[NotBuiltIn] ||
            // If we're asking wrt a primitve type, no user type can subtype one
            (this.isInstanceOf[NotBuiltIn] && this.isInstanceOf[RefType]))
      /* TODO: Currently, since we don't parse type descriptors, if we determine it's not
       * a subtype, it may very well be because we have an incomplete hierarchy */
      Tri.U
    else Tri.F*/
}
object Type {
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
  
  trait NotBuiltIn extends Type
  trait Incomplete extends NotBuiltIn
  
  def apply(raw: RawType) = intern(raw)
  def apply(raws: StdTypeList) : Seq[Type] = {
    for(i <- 0 until raws.size)
      yield apply(raws.get(i))
  }
  def apply(klass: Class[_]) = intern(RawType.internClassName(klass.getName))
  
  implicit def wrap(raw: RawType) = intern(raw)
  implicit def unwrap(t: Type) = t.raw
  
  import Registry._
  private val typeRegistry = {
    type R = Registered[RawType, Type, MutableConcurrentMap]
    new R#Map with R#Registrar
  }
  private def intern(raw:RawType) : Type = typeRegistry registered raw match {
    case Is(t) => t
    case Was   =>
      log('warn) ("Previously registered type for "+ raw +" now unreferenced")
      registerIncomplete(raw)
    case Not   => registerIncomplete(raw)
  }
  // Not a pre-registered type; most importantly: must be a reftype ergo an OBJECT
  private def registerIncomplete(raw: RawType) : Type = {
    assert(!raw.isPrimitive)
    if(raw.isArray) {
      // Recurse through nested array types
      def drill(raw:RawType) : Instantiable#Array = {
        val t =
          if(raw.isArray) drill(raw.getComponentType)
          else Type(raw).asInstanceOf[Instantiable]
        t.Array
      }
      drill(raw)
    }
    else {
      // Trigger registry of the unknown lifted type and attempt to infer its attributes
      // TODO
      /*val loader = ClassLoader.getSystemClassLoader
      val klass = try {
        loader loadClass raw.getClassName
      } catch {
        case _:ClassNotFoundException => null
      }*/
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
  val BIG_INT = t.BIG_INT
  val BIG_DEC = t.BIG_DEC
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