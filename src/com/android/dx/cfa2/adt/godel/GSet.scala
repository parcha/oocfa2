package com.android.dx.cfa2.adt.godel

import java.math.BigInteger.ONE

import scala.collection.generic.CanBuildFrom
import scala.collection._

/** Godel-hashing wrapper around an existing set **/
sealed abstract class GSet[E <: D#Hashed[E], D <: GodelHashDomain[E, D]]
(val self: GenSet[E], _godelHash: => GodelHash, protected[this] val hasBeenHashed: Boolean)
extends Set[E] with SetLike[E, GSet[E,D]]/* with GodelHashed[D, GSet[E, D]]*/ {
  lazy val godelHash = _godelHash
  // HACK: Scala doesn't have the type niceties Fortress has :/
  override def empty: GSet[E,D] = Empty.asInstanceOf[GSet[E,D]]
  def iterator = self.iterator
}

object GSet {
  private[godel] val (unhashWatermark, forgetWatermark, hashWatermark) = (8,16,32)
  implicit def apply[E <: D#Hashed[E], D <: GodelHashDomain[E, D]]
                    (self: GenSet[E]) = self match {
    case self:GSet[E,D] => self
    case _ =>
      if(self.size < hashWatermark) new Unhashed[E, D](self)()
      else new Hashed[E, D](self, godelHashForSet[E, D](self))
  }
  private[godel] def godelHashForSet[E <: D#Hashed[E], D <: GodelHashDomain[E, D]]
                                    (s: GenSet[E]): GodelHash = s.size match {
    case 0 => ONE
    case 1 => s.head.godelHash
    case _ => s map {_.godelHash} reduce {_ * _}
  }
}

private object Empty extends Unhashed[Nothing,Nothing](Set())(ONE)
private sealed class Unhashed[E <: D#Hashed[E], D <: GodelHashDomain[E, D]]
(self: GenSet[E], _hasBeenHashed: Boolean = false)
// By-name here in case we're not at the threshold
(_godelHash: => GodelHash = GSet.godelHashForSet[E,D](self))
extends GSet[E, D](self, _godelHash, _hasBeenHashed) {
  def contains(e) = self contains e
  def +(e) = {
    val newself = self + e
    if(newself.size < GSet.hashWatermark)
      new Unhashed[E,D](newself)()
    else new Hashed[E,D](newself, godelHash * e.godelHash)
  }
  def -(e) = {
    val newself = self - e
    if(newself.size <= GSet.forgetWatermark || !hasBeenHashed)
      new Unhashed[E,D](newself)()
    else new Unhashed[E,D](newself, true)(godelHash / e.godelHash)
  }
}

private final class Hashed[E <: D#Hashed[E], D <: GodelHashDomain[E, D]]
(self: GenSet[E], _godelHash: GodelHash) extends GSet[E, D](self, _godelHash, true) {
  def contains(e) = (godelHash % e.godelHash) == 0
  def +(e) = new Hashed[E,D](self + e, godelHash * e.godelHash)
  def -(e) = {
    val newself = self - e
    if(newself.size <= GSet.unhashWatermark)
      new Unhashed[E,D](newself, true)(godelHash / e.godelHash)
    else new Hashed[E,D](newself, godelHash / e.godelHash)
  }
  
  override def ++(s) = s match {
    case s:GenSet[E] => union(GSet[E,D](s))
    case _           => union(GSet[E,D](s.toSet))
  }
  override def --(s) = s match {
    case s:GenSet[E] => intersection(GSet[E,D](s))
    case _           => intersection(GSet[E,D](s.toSet))
  }
  def union(s: GSet[E,D]) = new Hashed[E,D](self union s.self, godelHash LCM s.godelHash)
  def intersection(s: GSet[E,D]) = {
    val newself = self intersect s.self
    if(newself.size <= GSet.unhashWatermark)
      new Unhashed[E,D](newself)()
    else new Hashed[E,D](newself, godelHash GCD s.godelHash)
  }
  def diff(s: GSet[E,D]) = {
    val newself = self diff s.self
    if(newself.size <= GSet.unhashWatermark)
      new Unhashed[E,D](newself)()
    else new Hashed[E,D](newself, godelHash / (godelHash GCD s.godelHash))
  }
  def subsetOf(s: GSet[E,D]) = (godelHash % s.godelHash) == 0
}