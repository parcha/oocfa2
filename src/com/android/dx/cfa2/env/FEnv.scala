package com.android.dx.cfa2.env

import com.android.dx
import dx.cfa2
import cfa2._
import `var`._
import `val`._

import scala.collection._

/** The environment of a set of fields */
sealed abstract class FEnv[Spec <: FieldSpec, F <: Var.Field[Instantiable, Spec]] extends Env[F] {
  final def get(spec:Spec) : Option[Val_] = {
    for((f,v) <- this)
      if(f.spec == spec) return Some(v)
    return None
  }
  final def getOrElse(spec:Spec, default: =>Val_) = get(spec) match {
    case Some(v) => v
    case None    => default
  }
  final def apply(spec:Spec) = get(spec).get
  final def contains(spec:Spec) = this exists {_._1.spec == spec}
  
  // FIXME: Hack relying on FieldSpec = SFieldSpec = IFieldSpec
  final def get(slot:FieldSlot) : Option[Val_] = get(slot.spec.asInstanceOf[Spec])
  final def getOrElse(slot:FieldSlot, default: =>Val_) : Val_ =
    getOrElse(slot.spec.asInstanceOf[Spec], default)
  final def apply(slot:FieldSlot) = get(slot).get
  final def contains(slot:FieldSlot) : Boolean = contains(slot.spec.asInstanceOf[Spec])
}

/** The environment of a set of static fields */
final class SFEnv
(final val self: SFEnv.M = SFEnv.defaultM)
extends FEnv[SFieldSpec, Var.StaticF_] with immutable.MapProxy[Var.StaticF_, Val_]
with SFEnv.ProxyFactoried {
  def union(that: SFEnv) = SFEnv.union(this, that)
}
object SFEnv extends EnvFactory[Var.StaticF_, SFEnv](new SFEnv(_)) {
  val defaultM = immutable.HashMap.empty[Var.StaticF_, Val_]
}

/** The environment of a set of instance fields */
final class IFEnv
(final val self: IFEnv.M = IFEnv.defaultM)
extends FEnv[IFieldSpec, Var.InstanceF_] with immutable.MapProxy[Var.InstanceF_, Val_]
with IFEnv.ProxyFactoried {
  def union(that: IFEnv) = IFEnv.union(this, that)
}
object IFEnv extends EnvFactory[Var.InstanceF_, IFEnv](new IFEnv(_)) {
  val defaultM = immutable.HashMap.empty[Var.InstanceF_, Val_]
}