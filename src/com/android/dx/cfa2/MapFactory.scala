package com.android.dx.cfa2

import scala.collection._

abstract class MapFactory[_K, _V, _M <: MapFactory.Mapped[_K, _V, _M]]
(private val empty: _M) { factory_ =>
  final type K = _K
  final type V = _V
  final type M = _M
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
  type Mapped[K,V,M <: Mapped[K,V,M]] = GenMap[K,V] with GenMapLike[K,V,M]
}

abstract class MapProxyFactory[K, V, M <: MapProxyFactory.Mapped[K,V,M], _Proxy <: MapProxyLike[K,V,M]]
(empty: M, private val proxyCtor: M => _Proxy)
extends MapFactory[K, V, M](empty) { factory_ =>
  final type Proxy = _Proxy
  final type ProxyLike = MapProxyLike[K,V,M]
  
  final implicit def wrap(m: M): Proxy =  proxyCtor(m)
  final implicit def unwrap(prox: Proxy): M = prox.self
  
  trait ProxyFactoried extends Proxy.Typed[M] { _:Proxy =>
    final val ++# = factory_.++#(self) _
    final val +# = factory_.+#(self) _
    final val factory = factory_
  }
}
object MapProxyFactory {
  type Mapped[K,V, M <: Mapped[K,V,M]] = Map[K,V] with MapLike[K,V,M]
}