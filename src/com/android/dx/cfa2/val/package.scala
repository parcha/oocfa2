package com.android.dx.cfa2

import com.android.dx
import dx.cfa2
import cfa2._
import tlc._
import Algebra._

import scala.collection._

package object `val` {
  
  type IParams = immutable.Map[Symbol, Any]
  
  type Val_ = Val[Instantiable]
  
  type VAL[+T <: Instantiable] = T#Value
  type VAL_ = VAL[Instantiable]
  
  type INST[+T <: Instantiable] = T#Instance
  type INST_ = INST[Instantiable]
  
  type SUBV[+T <: Instantiable] = V forSome {type V <: VAL[T]}
  type SUPV[-T <: Instantiable] = V forSome {type V >: VAL[T]}
  
  type ?[+T <: Instantiable] = UNKNOWN[T]
  type UNKNOWN[+T <: Instantiable] = T#Unknown
  type UNKNOWN_ = UNKNOWN[Instantiable]
  type KNOWN[+T <: Instantiable] = INST[T]
  type KNOWN_ = INST_
  
  type SUBT[T <: Type] = Sub forSome {type Sub <: T}
  
  /*
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
