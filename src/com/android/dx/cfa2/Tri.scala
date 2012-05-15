package com.android.dx.cfa2

import scala.collection._

/**
 * Kleene's trinary logic
 */
sealed abstract class Tri extends Immutable with NotNull with Serializable {
  import Tri._
  final def &(o: =>Tri) = this match {
    case T => o
    case F => F
    case U => o match {
      case F => F
      case _ => U
    }
  }
  final def |(o: =>Tri) = this match {
    case T => T
    case F => o
    case U => o match {
      case T => T
      case _ => U
    }
  }
  final def ^(o:Tri) = (this & !o) | (!this & o) 
  final def unary_! = this match {
    case T => F
    case F => T
    case U => U
  }
}
object Tri {
  case object T extends Tri
  case object F extends Tri 
  case object U extends Tri
  implicit def liftBoolean(b:Boolean) =
    if(b) T
    else  F
  implicit def lowerTri(t:T.type) = true
  implicit def lowerTri(f:F.type) = false
  
  /*
   *  Collective operations
   *  Key: op_ => could it be true?; _op => must it be true?
   */
  def any  (coll: GenIterable[Tri]) = coll reduce {_ | _}
  def any_ (coll: GenIterable[Tri]) = any(coll) != F
  def _any (coll: GenIterable[Tri]) = any(coll) == T
  
  def all  (coll: GenIterable[Tri]) = coll reduce {_ & _}
  def all_ (coll: GenIterable[Tri]) = all(coll) != F
  def _all (coll: GenIterable[Tri]) = all(coll) == T
  
  def none  (coll: GenIterable[Tri]) = !any(coll)
  def none_ (coll: GenIterable[Tri]) = none(coll) != F
  def _none (coll: GenIterable[Tri]) = none(coll) == T
  
  /** Returns:
   * * unknown iff all in coll are unknown
   * * true if at least one is true
   * * false if at least one is false
   * It is asserted that any which are not unknown must all agree on true or false
   */
  def concensus(coll: GenIterable[Tri]) : Tri = {
    var ans: Tri = Tri.U
    for(tri <- coll)
      if(ans == Tri.U && tri != Tri.U)
        ans = tri
      else
        assert(tri == Tri.U || tri == ans)
    ans
  }
  def concensus(tris: Tri*) : Tri = concensus(tris)
}