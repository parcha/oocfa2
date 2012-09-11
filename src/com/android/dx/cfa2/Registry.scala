package com.android.dx.cfa2

import scala.collection._
import scala.ref.WeakReference

trait Registry[K, V <: AnyRef] extends mutable.Map[K, WeakReference[V]] with Serializable {
  import Registry._
  def register(k:K, v:V) = this += ((k, new WeakReference(v)))
  def registered(k:K) = this get k match {
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
  def isRegistered(k:K) = registered(k).isInstanceOf[Is[V]]
  def wasRegistered(k:K) = registered(k) == Was
}
object Registry {
  sealed abstract class RegistrationStatus[+V]
  final case object Not extends RegistrationStatus[Nothing]
  final case object Was extends RegistrationStatus[Nothing]
  final case class Is[+V](v:V) extends RegistrationStatus[V]
  
  implicit def optionize[V](stat:RegistrationStatus[V]) = stat match {
    case Not   => None
    case Was   => None
    case Is(v) => Some(v)
  }
}