package com.android.dx.cfa2

import scala.collection._
import scala.ref.SoftReference

trait Cache[K, V <: AnyRef] extends mutable.Map[K, SoftReference[V]] {
  def cache(k:K, v:V) = {this += ((k, new SoftReference(v))); v}
  def cached(k:K) = this get k match {
    case None    => None
    case Some(r) => r.get
  }
  def cachedOrElse(k:K, otherwise: =>V) = cached(k) match {
    case Some(v) => v
    case None    => otherwise
  }
  def isCached(k:K) = cached(k) != None
}