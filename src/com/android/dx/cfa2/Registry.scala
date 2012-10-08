package com.android.dx.cfa2

import scala.collection._
import scala.ref.WeakReference

trait Registry[K, V <: AnyRef] extends mutable.Map[K, WeakReference[V]] with Serializable {
  
  sealed abstract class RegistrationStatus extends NotNull
  final case object Not extends RegistrationStatus
  final case object Was extends RegistrationStatus
  final case class Is(v:V) extends RegistrationStatus
  
  implicit def optionize(stat:RegistrationStatus) = stat match {
    case Not   => None
    case Was   => None
    case Is(v) => Some(v)
  }
  
  def register(k:K, v:V): Unit = this += ((k, new WeakReference(v)))
  def registered(k:K): RegistrationStatus = this get k match {
    case None    => Not
    case Some(r) => r.get match {
      case None    => Was
      case Some(v) => Is(v)
    }
  }
  def registeredOrElse(k:K, otherwise: =>V): V = registered(k) match {
    case Is(v)     => v
    case Not | Was => otherwise
  }
  def isRegistered(k:K) = registered(k).isInstanceOf[Is]
  def wasRegistered(k:K) = registered(k) == Was
}  
trait Registrar[K, V<:AnyRef] {
  protected[this] type RegistryMap = MutableConcurrentMap[K,WeakReference[V]]
  protected[this] object registry extends RegistryMap with Registry[K,V]
}