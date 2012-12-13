package com.android.dx.cfa2

import collection.{parallel=>par, _}
import collection.JavaConversions._
import collection.mutable._
import collection.concurrent
import concurrent.TrieMap

class MutableConcurrentMap[K,V]
extends mutable.MapProxy[K,V] with concurrent.Map[K,V] {
  val self = new TrieMap[K,V]
  /* Manual forwarding */
  def putIfAbsent(k, v) = self.putIfAbsent(k,v)
  def remove(k, v) = self.remove(k,v)
  def replace(k, v) = self.replace(k, v)
  def replace(k, oldV, newV) = self.replace(k, oldV, newV)
}

final class MutableConcurrentMultiMap[K,V]
extends MutableConcurrentMap[K, SynchronizedSet[V]] {
  def += (k: K, v: V): Unit = {
    if(!(this contains k))
      this += ((k, new HashSet[V] with SynchronizedSet[V]))
    this(k) += v
  }
}