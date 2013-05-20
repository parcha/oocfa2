package com.android.dx.cfa2.adt

import scala.collection._
import scala.ref.SoftReference
import com.android.dx.cfa2.adt.MutableConcurrentMap

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
trait Cacher[K, V<:AnyRef] {
  protected[this] type CacheMap = MutableConcurrentMap[K,SoftReference[V]]
  protected[this] object cache extends CacheMap with Cache[K,V]
}