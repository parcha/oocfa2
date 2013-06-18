package com.android.dx.cfa2
package cfa2.env

import env._
import wrap._
import tlc.Algebra._
import scala.collection.GenSet
import scala.collection.immutable._
import _Heap._
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._

trait _Heap[+Self <: _Heap[Self, _Scheme], _Scheme <: HeapScheme]
extends MapLike[_Scheme#Address, _Scheme#Referent, Self]
with Map[_Scheme#Address, _Scheme#Referent] with Immutable with NotNull { _:Self =>
  final type Scheme = _Scheme
  final type Address = Scheme#Address
  final type Referent = Scheme#Referent
  final type Allocator = Scheme#Allocator
  implicit val scheme: Scheme

  /* Disambiguate some overrides */
  override def empty: Self = ???
  override final def newBuilder = ???
}

trait HeapScheme extends NotNull {
  type Address
  implicit val _Address: WeakTypeTag[Address]
  type Referent
  type Allocator <: {
    type Range = Address
  }
}
trait RegionScheme[_ShellScheme <: HeapScheme] extends HeapScheme {
  final type ShellScheme = _ShellScheme 
  type Address <: ShellScheme#Address
  type Referent <: ShellScheme#Referent
}
trait LeafScheme[ShellScheme <: HeapScheme] extends RegionScheme[ShellScheme] {
  final type Map_ = Map[Address, Referent]
}
trait WrappedLeafScheme[ShellScheme <: HeapScheme] extends RegionScheme[ShellScheme] {
  final type Map_ = Map[Address_, Referent]
  type Address_
  implicit def unwrapAddress(addr: Address): Address_
  implicit def wrapAddress(addr: Address_): Address
}

object _Heap {
  type Heap[Scheme <: HeapScheme] = Self forSome { type Self <: _Heap[Self, Scheme]}
  type Region[ShellScheme <: HeapScheme] = Heap[RegionScheme[ShellScheme]]
  type Regions[ShellScheme <: HeapScheme] = GenSet[Region[ShellScheme]]
  
  abstract case class Shell[+Self <: Shell[Self, Scheme], Scheme <: HeapScheme]
  (regions: Regions[Scheme])(implicit val scheme: Scheme)
  extends _Heap[Self, Scheme] { _:Self =>
    import scheme._
    assert(regions.size > 1)
    // Each region's Address should be incomparable to the others
    private[this] def subsumedAddress(r: Region_) =
      regions exists {r.scheme._Address.tpe <:< _.scheme._Address.tpe}
    assert(!(regions exists subsumedAddress))
    
    protected[this] final type Region_ = Region[Scheme]
    protected[this] final type Regions_ = Regions[Scheme]
    
    @inline
    protected[this] def constructor(regions: Regions_): Self
    protected[this] final val partitioned = Map(
        ( for(r <- regions) yield (r.scheme._Address, r) ).seq.toSeq: _* )
    /*protected[this] final type Regioned[A <: Address] = Region_ {
      type Address = A
    }*/
    @inline
    protected[this] final def getRegion[A <: Scheme#Address](addr: A): Region_ =
      partitioned find {scheme._Address.tpe <:< _._1.tpe} match {
        case Some((_, region)) => region
        case None => ??? //TODO: Put in a proper internal error
      }
    /** Convenience functions for COW-transforming this Shell **/
    @inline
    protected[this] final def replaceRegion(from: Region_, to: Region_): Self =
      constructor((regions - from) + to)
    @inline
    protected[this] final def transformRegion(from: Region_)(tr: (from.type)=>Region_): Self =
      replaceRegion(from, tr(from))
    /** Convenience functions to force a correct down-cast **/
    @inline
    protected[this] final def addr_[A <: Address](addr: Address): A = addr.asInstanceOf[A]
    @inline
    protected[this] final def ref_[R <: Referent](ref: Referent): R = ref.asInstanceOf[R]
    @inline
    protected[this] final def kv_[H <: Region_, R >: Referent](kv: (Address, R)) =
      kv.asInstanceOf[(H#Address, H#Referent)]
        
    final def get(addr) = getRegion(addr) get addr_(addr)
    final lazy val iterator = regions map {_.iterator} reduce {_ ++ _}
    final def +[R >: Referent](kv) = {
      val addr = kv._1
      val region = getRegion(addr)
      val to = (region + kv_(kv)).asInstanceOf[Region_]
      replaceRegion(region, to)
    }
    final def -(addr): Self = transformRegion(getRegion(addr)) {_ - addr_(addr)}
  }
  
  abstract case class Leaf[+Self <: Leaf[Self, Scheme], Scheme <: LeafScheme[_]]
  (proxy: Scheme#Map_)(implicit val scheme: Scheme)
  extends _Heap[Self, Scheme] { _:Self =>
    @inline
    protected[this] def constructor(proxy: Scheme#Map_): Self
    /** Convenience functions to force a correct down-cast **/
    @inline
    protected[this] final def addr_[A <: Address](addr: Address): A = addr.asInstanceOf[A]
    @inline
    protected[this] final def kv_[K <: Address, V <: Referent, R >: Referent](kv: (Address,R)) =
      kv.asInstanceOf[(K,V)]
    
    final def get(addr) = proxy get addr_(addr)
    final def iterator = proxy iterator
    final def +[R >: Referent](kv) = constructor(proxy + kv_(kv))
    final def -(addr): Self = constructor(proxy - addr_(addr))
    override final def empty: Self = constructor(proxy.empty)
  }
  
  abstract case class WrappedLeaf[+Self <: WrappedLeaf[Self, Scheme], Scheme <: WrappedLeafScheme[_]]
  (proxy: Scheme#Map_)(implicit val scheme: Scheme)
  extends _Heap[Self, Scheme] { _:Self =>
    import scheme._
    @inline
    protected[this] def constructor(proxy: Scheme#Map_): Self
    /** Convenience functions to force a correct down-cast **/
    @inline
    protected[this] final def addr_[A <: Scheme#Address_](addr: Address): A =
      unwrapAddress(addr.asInstanceOf[scheme.Address]).asInstanceOf[A]
    @inline
    protected[this] final def _addr[A <: Scheme#Address_](addr: A): Address =
      wrapAddress(addr.asInstanceOf[scheme.Address_])
    @inline
    protected[this] final def kv_[K <: Address, V <: Referent, R >: Referent](kv: (Address,R)) =
      kv.asInstanceOf[(K,V)]
    
    final def get(addr) = proxy get addr_(addr)
    final lazy val iterator = proxy map {kv=>(_addr(kv._1), kv._2)} iterator
    final def +[R >: Referent](kv) = constructor(proxy + kv_(kv))
    final def -(addr): Self = constructor(proxy - addr_(addr))
    override final def empty: Self = constructor(proxy.empty)
  }
}