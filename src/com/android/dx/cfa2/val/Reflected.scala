package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._
import CFA2Analysis.singleton.opts.log
import scala.collection.{parallel => par, _}
import scala.ref.WeakReference

trait Reflected[ET] extends Instantiable {
  import Reflected._
  type EigenType = ET
  implicit val EigenType_ : ClassManifest[EigenType]
  
  require(EigenType_ != null)
  if(this.isInstanceOf[OBJECT])
    require(this.asInstanceOf[OBJECT].klass == EigenType_.erasure)
  reflRegistry.register(EigenType_, this)
  
  // Just a default
  def instance(self:EigenType, deps: Val_) : Instance = constructor(paramify(('self, self)), deps)
  final def instance(self:EigenType) : Instance = instance(self, Val.Bottom)
  implicit final def deconstruct(inst: Self#Instance) = inst.self
  
  instance_param[ET]('self, {_!=null})
  protected[this] trait Instance_ extends super.Instance_ { _:Instance =>
    final lazy val self = param[ET]('self) 
  }
  type Instance <: Instance_
}
object Reflected {
  import Registry._
  private val reflRegistry = {
    type R = Registered[ClassManifest[_], Reflected[_], MutableConcurrentMap]
    new R#Map with R#Registrar
  }
  
  def apply[ET](implicit ET_ : ClassManifest[ET]) : Option[Reflected[ET]] =
    reflRegistry registered ET_ match {
      case Not   => None
      case Was   =>
        log('warn) ("Previously registered reflected type "+ ET_ +" now unreferenced")
        None
      case Is(t) => Some(t.asInstanceOf[Reflected[ET]])
    }
  def apply[ET](klass: Class[ET]) : Option[Reflected[ET]] =
    apply[ET](ClassManifest.fromClass(klass))
  def apply[ET](eigenval: ET) : Option[R forSome {type R <: Reflected[ET]}] =
    apply[ET](eigenval.getClass.asInstanceOf[Class[ET]])
  
  implicit def reflect[ET](v:ET) : R#Instance forSome {type R <: Reflected[ET]} =
    apply[ET](v).get.instance(v, Val.Bottom)
  implicit def lift[ET, R <: Reflected[ET]](refl:R#Instance) : ET = refl.self
}