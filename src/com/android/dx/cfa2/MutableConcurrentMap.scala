package com.android.dx.cfa2

import collection.{parallel=>par, _}
import collection.JavaConversions._
import collection.mutable._
import java.util.concurrent.ConcurrentHashMap

class MutableConcurrentMap[K, V]
extends JConcurrentMapWrapper[K, V](new ConcurrentHashMap)
//extends HashMap[K, V] with SynchronizedMap[K, V]

final class MutableConcurrentMultiMap[K,V]
extends MutableConcurrentMap[K, SynchronizedSet[V]] {
  def += (k: K, v: V): Unit = {
    if(!(this contains k))
      this += ((k, new HashSet[V] with SynchronizedSet[V]))
    this(k) += v
  }
}