package com.android.dx.cfa2.adt.godel

import java.math.BigInteger
import scala.util.hashing.Hashing

final class GodelHash private[godel] (val n: BigInteger) extends AnyVal {
  def +(g: GodelHash): GodelHash = n add g
  def -(g: GodelHash): GodelHash = n subtract g
  def *(g: GodelHash): GodelHash = n multiply g
  def /(g: GodelHash): GodelHash = n divide g
  def %(g: GodelHash): GodelHash = n remainder g
  def GCD(g: GodelHash): GodelHash = n gcd g
  def LCM(g: GodelHash): GodelHash = this * (g / (this GCD g))
}
object GodelHash {
  implicit def unwrap(g: GodelHash): BigInteger = g.n
  private[godel] implicit def wrap(i: BigInteger): GodelHash = new GodelHash(i)
}

final class GodelIndex private[godel] (val n: Int) extends AnyVal {
  def hash: GodelHash = new GodelHash(ProbablePrimeFactory(n))
  def +(g: GodelHash): GodelHash = hash + g
  def -(g: GodelHash): GodelHash = hash - g
  def *(g: GodelHash): GodelHash = hash * g
  def /(g: GodelHash): GodelHash = hash / g
  def %(g: GodelHash): GodelHash = hash % g
  def GCD(g: GodelHash): GodelHash = hash GCD g
  def LCM(g: GodelHash): GodelHash = hash LCM g
}
object GodelIndex {
  implicit def godelHash(idx: GodelIndex) = idx.hash
}

trait GodelIndexing[T] extends Hashing[T] {
  final def godelIndex(x: T) = new GodelIndex(hash(x)) 
}

abstract class GodelHashDomain[+E <: Self#Hashed[E], Self <: GodelHashDomain[E, Self]] {
  _:Self =>
  def next: GodelHash
  trait Hashed[+HSelf <: Hashed[HSelf]] { _:HSelf =>
    type Domain = Self
    def godelHash: GodelHash
  }
}
abstract class GodelIndexDomain[+E <: Self#Indexed[E], Self <: GodelIndexDomain[E, Self]]
extends GodelHashDomain[E, Self] { _:Self =>
  final def next = nextIdx
  def nextIdx: GodelIndex
  trait Indexed[+ISelf <: Indexed[ISelf]] extends Hashed[ISelf] { _:ISelf =>
    final def godelHash = godelIndex.hash
    val godelIndex: GodelIndex
  }
}
object GodelHashDomain {
  // FIXME: This actually won't behave b/c the mapping needs to be bijective
  class Monotonic[+E <: Self#Indexed[E], Self <: Monotonic[E, Self]]
  extends GodelIndexDomain[E, Self] { _:Self =>
    private[this] var _nextIdx = 0
    def nextIdx = { 
      val i = _nextIdx
      _nextIdx += 1
      new GodelIndex(i)
    }
  }
  /*class Allocated[+E <: GodelHashed[Self, E], Self <: Allocated[E, Self]]
  extends GodelIndexDomain[E, Self]*/
}