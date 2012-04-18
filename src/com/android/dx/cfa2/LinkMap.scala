package com.android.dx.cfa2

import scala.collection._

/** Linked maps which are composed of a linked chain of maps */
trait LinkMap[K, +V, +P <: Map[K, V]] extends Map[K,V] {
  val parent: P
  final abstract override def get(key:K) : Option[V] =
    super.get(key) match {
      case Some(value) => Some(value)
      case None        => if(parent == null) None else parent get key
    }
}