package com.android.dx.cfa2.tlc

object Algebra {
  
  // Negation
  type ![T] = T => Nothing
  
  // Double-negation
  type !![T] = ![![T]]
  
  // Binary union
  type | [A, B] = { type ^ [T] = !![T] <:< (A ||| B) }
  type ||| [A, B] = ![![A] & ![B]]
  
  // N-ary union
  trait Disj[T] {
    type | [S] = Disj[T & ![S]]
    type ^ = ![T]
  }
  // for convenience
  type UNION[T] = { type | [S] = Disj[![T]]# | [S] }
  
  /*trait Union [A <: U, B <: U, U] {
    protected val a:A
    protected val b:B
    protected val u : A|B|U
  }
  class Union_ [A <: U, B <: U, U](protected val a:A, protected val b:B)
  extends Union[A, B, U] {
    
  }*/
  
  // Intersection
  type & [A, B] = A with B
  
}