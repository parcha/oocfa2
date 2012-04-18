package com.android.dx.cfa2.tlc

import NAT._

sealed trait HList {
  import HList._
  
  type FoldR [Init <: Type, Type, F <: FOLD[_, Type]] <: Type
  def :^ [Init <: Type, Type, F <: FOLD[Any, Type]] (i: Init, f: F) : (FoldR [Init, Type, F])
  
  type FoldL [Init <: Type, Type, F <: FOLD[_, Type]] <: Type
  def ^: [Init <: Type, Type, F <: FOLD[Any, Type]] (i: Init, f: F) : (FoldL [Init, Type, F])
  
  type Length[H <: HList] =
    H# FoldR [_0, NAT, `+1`]

  type Append [A <: HList, B <: HList] =
    A# FoldR [B, HList, AppHCons.type]
   
  type Reverse_Append [A <: HList, B <: HList] =
    A# FoldL [B, HList, AppHCons.type]
   
  type Reverse[A <: HList] =
    A# FoldL [HNil.type, HList, AppHCons.type]

  object AppHCons extends FOLD[Any, HList] {
    type \ [A <: Any, B <: HList] = A Cons B
    def apply[A, B <: HList](a: A, b: B) = HCons(a, b)
  }

}

sealed class HNil extends HList {
   def +>[T](v : T) = HCons(v, this)
   
   type FoldR [Init <: Type, Type, F <: FOLD[_, Type]] = Init
   def :^ [Init <: Type, Type, F <: FOLD[_, Type]] (i: Init, f: F) : (FoldR [Init, Type, F]) = i
   
   type FoldL [Init <: Type, Type, F <: FOLD[_, Type]] = Init
   def ^: [Init <: Type, Type, F <: FOLD[_, Type]] (i: Init, f: F) : (FoldL [Init, Type, F]) = i
}
case object HNil extends HNil

final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
   def +>[T](v : T) = HCons(v, this)
   
   type FoldR [Init <: Type, Type, F <: FOLD[_, Type]] = F# \ [H, tail.FoldR [Init, Type, F]]
   def :^ [Init <: Type, Type, F <: FOLD[Any, Type]] (i: Init, f: F) : (FoldR [Init, Type, F]) =
     f(head, tail.:^ [Init, Type, F](i, f))
     
   type FoldL [Init <: Type, Type, F <: FOLD[_, Type]] = tail.FoldL [F# \ [H, Init], Init, Type]
   def ^: [Init <: Type, Type, F <: FOLD[Any, Type]] (i: Init, f: F) : (FoldL [Init, Type, F]) =
     tail.^: [F# \ [H, Init], Type, F](f(head, i), f)
}

// aliases for building HList types and for pattern matching
object HList {
  type Cons [H, T <: HList] = HCons[H, T]
  val +> = HCons
}


/*sealed trait HListOps[B <: HList] {
   def length: Int
   def +>> [A <: HList](a: A): A +>> B
   def reverse: Reverse[B]
   def reverse_+>> [A <: HList](a: A): A Reverse_+>> B
}

object HListOps {
implicit def hlistOps[B <: HList](b: B): HListOps[B] =
   new HListOps[B] {
   
      def length =
         b.foldr(Length, 0)

      def reverse =
         b.^: [HNil.type, HList, AppHCons.type](HNil, AppHCons)

      def +>> [A <: HList](a: A): A#Foldr[HList, AppHCons.type, B] =
         a.:^ [B, HList, AppHCons.type](b, AppHCons)

      def reverse_+>> [A <: HList](a: A): A Reverse_+>> B =
         a.^: [B, HList, AppHCons.type](b, AppHCons)
   }
}
   
object Length extends Fold[Any, Int] {
   type Apply[N <: Any, Acc <: Int] = Int
   def apply[A,B <: Int](a: A, b: B) = b+1
}*/