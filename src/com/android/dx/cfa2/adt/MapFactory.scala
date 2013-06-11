package com.android.dx.cfa2.adt

import scala.collection._

abstract class MapFactory[K, +V, +_M[K,V] <: MapFactory.Mapped[K, V, _M[K,V]]]
(private[this] val empty: _M[K,V]) { factory_ =>
  final type M = _M[K,V]
  class Builder(start:M = empty) extends mutable.MapBuilder[K, V, M](start) /*with MapLike[K, V, M]*/
  implicit object Builder extends generic.CanBuildFrom[M, (K, V), M] {
    def apply() = new factory_.Builder()
    def apply(from: M) = new factory_.Builder(from)
  }
  
  trait Factoried { _:M =>
    final val ++# = factory.++#(this) _
    final val +# = factory.+#(this) _
    final val factory = factory_
  }
  
  @inline
  final def ++# (self:M)(m:Map[K,V]) : M = self.++[(K, V), M](m)
  @inline
  final def +# (self:M)(k:K, v:V) : M = self.++[(K, V), M](Array((k,v)))
}
object MapFactory {
  type Mapped[K,+V,+M <: Mapped[K,V,M]] = GenMap[K,V] with GenMapLike[K,V,M]
}

abstract class MapProxyFactory[K, +V, 
                               +M[K,V] <: MapProxyFactory.Mapped[K,V,M[K,V]],
                               +_Proxy <: MapProxyLike[K,V,M[K,V]]]
(empty: M[K,V], private[this] val proxyCtor: M[K,V] => _Proxy)
extends MapFactory[K, V, M](empty) { factory_ =>
  final type Proxy = _Proxy
  final type ProxyLike = MapProxyLike[K,V,M]
  
  /** CAUTION: Inherit this BEFORE all other traits, otherwise you get recursion **/
  trait ProxyFactoried { _:Proxy =>
    final val ++# = factory.++#(self) _
    final val +# = factory.+#(self) _
    final val factory = factory_
  }
  
  final implicit def wrap(m: M): Proxy =  proxyCtor(m)
}
object MapProxyFactory {
  type Mapped[K,+V, +M <: Mapped[K,V,M]] = GenMap[K,V] with GenMapLike[K,V,M]
}