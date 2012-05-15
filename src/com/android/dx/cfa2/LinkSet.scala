package com.android.dx.cfa2

import scala.collection._

trait LinkSet[E, +P <: Set[E]] extends Set[E] with Serializable {
  val parent: P
  abstract override def contains(elem:E) =
    if(super.contains(elem)) true
    else if(parent != null) parent.contains(elem)
    else false
  abstract override def iterator =
    if(parent == null) super.iterator
    else (super.iterator ++ parent.iterator).toSeq.distinct.iterator
}