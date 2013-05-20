package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._
import adt.Registrar
import analysis.CFA2Analysis.singleton.opts.log
import scala.reflect.ClassTag
import scala.collection.{parallel => par, _}
import scala.ref.WeakReference

trait Reflected[ET] extends Instantiable with DelayedInit {
  import Reflected._
  type EigenType = ET
  implicit val EigenType_ : ClassTag[EigenType]
  override final val klass = EigenType_.runtimeClass
  
  require(EigenType_ != null)
  if(this.isInstanceOf[OBJECT])
    require(this.asInstanceOf[OBJECT].klass == EigenType_.runtimeClass)
  register(EigenType_, this)
  
  // Just a default
  def instance(self:EigenType, deps: Val_) : Instance = constructor(paramify(('self, Some(self))), deps)
  final def instance(self:EigenType) : Instance = instance(self, Val.Bottom)
  implicit final def deconstruct(inst: Self#Instance) = inst.self
  
  /**
   * Default, convenience value of EigenType; i.e. what you'd get from a nullary constructor
   * Set to None if there is no such value
   **/
  protected[this] def defaultSelf: Option[ET]
  
  // Hack for proper initialization; really just want this one clause to be executed after
  // all other initialization phases, but without the if-statement, this would be executed
  // at every init @_@
  override def delayedInit(body) = {
  //super.delayedInit(body)
  body
  if(!isIParamRegistered('self)) {
    instance_param_[Option[ET]]('self, defaultSelf, {_!=null})
  }
  }
  protected[this] trait Instance_ extends super.Instance_ { _:Instance =>
    // We should not attempt to use self if there is not actually a self (e.g. unknown)
    final lazy val _self = param[Option[ET]]('self)
    final lazy val self = _self.get 
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