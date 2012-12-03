package com.android.dx.cfa2

import com.android.dx
import dx.cfa2
import cfa2._

import scala.collection._

package object `val` {
  
  type ARRAY[T <: Instantiable] = T#Array
  type ARRAY_ = ARRAY[Instantiable]
  
  type IParams = immutable.Map[Symbol, Any]
  
  type Val_ = Val[Instantiable]
  
  type VAL[+T <: Instantiable] = T#Value
  type VAL_ = VAL[Instantiable]
  
  type INST[T <: Instantiable] = T#Instance
  type INST_ = INST[Instantiable]
  
  type REF[T <: RefType] = INST[T]#Ref
  
  type SUBV[+T <: Instantiable] = V forSome {type V <: VAL[T]}
  type SUPV[-T <: Instantiable] = V forSome {type V >: VAL[T]}
  
  type UNKNOWN[T <: Instantiable] = T#Unknown
  type UNKNOWN_ = UNKNOWN[Instantiable]
  type KNOWN[T <: Instantiable] = INST[T]
  type KNOWN_ = INST_
  
  type SUBT[T <: Type] = Sub forSome {type Sub <: T}
  
  type Reflected_ = Reflected[_]
  type Dynamic_ = Dynamic[_]
  
  // Witnesses
  object Unknown_? {
    def unapply[T <: Instantiable](v:VAL[T]): Option[UNKNOWN[T]] =
      if(v.isUnknown) Some(v.asInstanceOf[UNKNOWN[T]])
      else None
  }
  object Known_? {
    def unapply[T <: Instantiable](v:VAL[T]): Option[KNOWN[T]] =
      if(!v.isUnknown) Some(v.asInstanceOf[KNOWN[T]])
      else None
  }
  object Null_? {
    def unapply(v:VAL[RefType]): Boolean = v match {
      case v if v.typ == NULL => true
      case Known_?(v) if v.isNull == Tri.T => true
      case _ => false
    }
  }
  object NonNull_? {
    def unapply(v:KNOWN[RefType]): Boolean =
      if(v.isNull == Tri.F) true
      else false
  }
  object Subtype_? {
    // FIXME: somehow make this only work for built-in types; a problem given Scala doesn't do type negation
    def unapply[V[_ <: Instantiable] <: VAL[_],
                T <: Instantiable : V]
               (v:V[_]): Option[V[T]] =
      if(v.typ.isInstanceOf[T]) Some(v.asInstanceOf[V[T]])
      else None
  }
  object OBJECT_? {
    def unapply[V[_ <: Instantiable] <: VAL[_]]
               (v:V[_]): Option[V[OBJECT]] =
      if(v.typ.isInstanceOf[OBJECT]) Some(v.asInstanceOf[V[OBJECT]])
      else None
  }
  object ARRAY_? {
    def unapply[V[_ <: Instantiable] <: VAL[_]]
               (v:V[_]): Option[V[ARRAY_]] = {
      if(v.typ.isInstanceOf[ARRAY_])
          Some(v.asInstanceOf[V[ARRAY_]])
      None
    }
  }
  object ARRAY__? {
    def unapply[V[_ <: Instantiable] <: VAL[_],
                A <: ARRAY[T] : V,
                T <: Instantiable]
               (v:V[_]): Option[V[A]] = {
      if(v.typ.isInstanceOf[ARRAY_] &&
         v.asInstanceOf[ARRAY_].component_typ.isInstanceOf[T])
          Some(v.asInstanceOf[V[A]])
      None
    }
  }
  
  /*
   * FIXME
   *  At the moment, we just alias this to any instantiable type because we
   *  can't just limit it to derivations of THROWABLE since when we lift a type
   *  we don't parse its descriptor to figure out if it is a THROWABLE
   */
  type Exceptional = Instantiable
  
  def makeUnknowns(types: Seq[Instantiable]) : Seq[Val[Instantiable]] =
    for(t <- types) yield Val.Unknown(t)
  
  // TODO: Hack for the non-existence of isNull for Unknown
  final def isPossiblyNull(v: VAL[RefType]) = v match {
    case v if v.isUnknown => Tri.U
    case v:KNOWN[RefType] => v.isNull 
  }
  
  /*type VOID = VOID.type
  type BOOLEAN = BOOLEAN.type
  type BYTE = BYTE.type
  type CHAR = CHAR.type
  type DOUBLE = DOUBLE.type
  type FLOAT = FLOAT.type
  type INT = INT.type
  type LONG = LONG.type
  type SHORT = SHORT.type
  
  type CLASS = CLASS.type
  type STRING = STRING.type
  type BIG_INT = BIG_INT.type
  type BIG_DEC = BIG_DEC.type
  type LOCALE = LOCALE.type*/
}
