package com.android.dx.cfa2.tlc

object Func {

  type TFunc1 = { type ^[X] }
  type TFunc2 = { type ^[X, Y] }
  type TFunc3 = { type ^[X, Y, Z] }
  
  type Apply[F <: TFunc1, X] = F# ^[X]
  // Compose
  type %[F <: TFunc1, G <: TFunc1] = {
    type ^[X] = F# ^[ G# ^[X] ]
  }
  
  import Algebra._
  // Test a type for conformance with a type expression
  type ~~ [X, T] = !![X] <:< T
  type ?~ [T] = ~~[_, T]
  type ~ [T] = {
    type ^[X] = ~~[X, T]
  }
  
  type << [X, T] = X <:< T
  type ?< [T] = <<[_, T]
  type < [T] = {
    type ^[X] = <<[X, T]
  }
  
  type >> [X, T] = T <:< X
  type ?> [T] = >>[_, T]
  type > [T] = {
    type ^[X] = >>[X, T]
  }
  
  type And2 [F <: TFunc2, A, B] = {
    type ^[X] = F# ^[X, A] with F# ^[X, B]
  }
  
  type Meet2 [A, B, Lower] = {
    type ^[X] = (X <:< A) with (X <:< B) with (Lower <:< X)
  }
  
  type Join2 [A, B, Upper] = {
    type ^[X] = (A <:< X) with (B <:< X) with
                (A <:< Upper) with (B <:< Upper) with (X <:< Upper)
  }
  
  class Test[U, T1 <: U, T2 <: U, J : Join2[T1, T2, U]# ^] {
    implicitly[J <:< U]
  }
  
}