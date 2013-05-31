package com.android.dx.cfa2.adt.godel

import java.math.BigInteger
import scala.collection.mutable._

object PrimeFactory {
  final class Prime private[PrimeFactory] (val n: BigInteger) extends AnyVal
  implicit def unwrap(p: Prime) = p.n
}
sealed abstract class PrimeFactory {
  import PrimeFactory._
  protected final val memo: ArrayBuffer[BigInteger] = new ArrayBuffer[BigInteger](100)
  final def apply(i: Int): Prime = {
    ensure(i+1)
    new Prime(memo(i))
  }
  def ensure(i: Int): Unit
}

object ProbablePrimeFactory extends PrimeFactory {
  def ensure(i) = {
    var cnt = memo.size
    while(cnt < i) memo += memo.last.nextProbablePrime
  }
}