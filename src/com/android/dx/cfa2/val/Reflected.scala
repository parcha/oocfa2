package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._
import CFA2Analysis.singleton.opts.log
import scala.reflect.ClassTag
import scala.collection.{parallel => par, _}
import scala.ref.WeakReference

trait Reflected[ET] extends Instantiable {
  import Reflected._
  type EigenType = ET
  implicit val EigenType_ : ClassTag[EigenType]
  
  require(EigenType_ != null)
  if(this.isInstanceOf[OBJECT])
    require(this.asInstanceOf[OBJECT].klass == EigenType_.runtimeClass)
  register(EigenType_, this)
  
  // Just a default
  def instance(self:EigenType, deps: Val_) : Instance = constructor(paramify(('self, self)), deps)
  final def instance(self:EigenType) : Instance = instance(self, Val.Bottom)
  implicit final def deconstruct(inst: Self#Instance) = inst.self
  
  protected[this] val default: ET
  
  instance_param_[ET]('self, default, {_!=null})
  protected[this] trait Instance_ extends super.Instance_ { _:Instance =>
    final lazy val self = param[ET]('self) 
  }
  type Instance <: Instance_
}
object Reflected extends Registrar[ClassTag[_], Reflected[_]] {
  import registry._
  private def register[ET](manifest: ClassTag[ET], refl: Reflected[ET]) =
    registry.register(manifest, refl)
  
  def apply[ET](implicit ET_ : ClassTag[ET]) : Option[Reflected[ET]] =
    registry registered ET_ match {
      case Not   => None
      case Was   =>
        log('warn) ("Previously registered reflected type "+ ET_ +" now unreferenced")
        None
      case Is(t) => Some(t.asInstanceOf[Reflected[ET]])
    }
  def apply[ET](klass: Class[ET]) : Option[Reflected[ET]] =
    apply[ET](ClassTag[ET](klass))
  def apply[ET](eigenval: ET) : Option[R forSome {type R <: Reflected[ET]}] =
    apply[ET](eigenval.getClass.asInstanceOf[Class[ET]])
  
  def reflect[ET](v:ET) : R#Instance forSome {type R <: Reflected[ET]} =
    apply[ET](v).get.instance(v, Val.Bottom)
  implicit def lift[ET, R <: Reflected[ET]](refl:R#Instance) : ET = refl.self
}