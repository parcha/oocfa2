package com.android.dx.cfa2

import scala.collection._

/**
 * Kleene's trinary logic
 */
sealed abstract class Tri extends Immutable with NotNull with Serializable {
  import Tri._
  def &(o: =>Tri) : Tri
  def |(o: =>Tri) : Tri
  final def ^(o:Tri) = (this & !o) | (!this & o) 
  val unary_! : Tri
  /** T | U **/
  val unary_+ : Boolean
  /** F | U **/
  val unary_- : Boolean
}
object Tri {
  case object T extends Tri {
    def &(o: =>Tri) = o
    def |(o: =>Tri) = this
    val unary_! = F
    val unary_+ = true
    val unary_- = false
  }
  case object F extends Tri {
    def &(o: =>Tri) = this
    def |(o: =>Tri) = o
    val unary_! = T
    val unary_+ = false
    val unary_- = true
  } 
  case object U extends Tri {
    def &(o: =>Tri) = o match {
      case F => F
      case _ => U
    }
    def |(o: =>Tri) = o match {
      case T => T
      case _ => U
    }
    val unary_! = U
    val unary_+ = true
    val unary_- = true
  }
  def apply(b: Boolean) = lift(b)
  implicit def lift(b:Boolean): Tri =
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