package com.android.dx.cfa2

import com.android.dx
import dx.cfa2
import cfa2._
import CFA2Analysis.singleton.opts.log
import tlc._
import Algebra._

import scala.collection._

package object `val` {
  
  type IParams = immutable.Map[Symbol, Any]
  
  type Val_ = Val[Instantiable]
  
  type VAL[+T <: Instantiable] = T#Value
  type VAL_ = VAL[Instantiable]
  type SUBV[+T <: Instantiable] = V forSome {type V <: VAL[T]}
  type SUPV[-T <: Instantiable] = V forSome {type V >: VAL[T]}
  type ?[+T <: Instantiable] = T#Unknown
  
  type SUBT[T <: Type] = Sub forSome {type Sub <: T}
  
  /*
   *  At the moment, we just alias this to any instantiable type because we
   *  can't just limit it to derivations of THROWABLE since when we lift a type
   *  we don't parse its descriptor to figure out if it is a THROWABLE
   */
  type Exceptional = Instantiable
  
  def makeUnknowns(types: Seq[Instantiable]) : Seq[Val[Instantiable]] =
    for(t <- types) yield Val.Unknown(t)
  
  // FIXME: What should this do wrt arrays?
  def reflectClass(name: String) : Option[Class[_]] =
    try Some(AnalysisClassLoader.loadClass(name))
    catch {
      case _:ClassNotFoundException =>
        log('warn) ("Couldn't find class for reflection: "+name)
        None
      case e:NoClassDefFoundError =>
        log('warn) ("In attempting to load "+name+" for reflection, a depedency could not be found: "+e.getMessage)
        None
    }
  
}