package com.android.dx.cfa2.tlc

import CMP._

sealed trait NAT {
  type ~[NonZero[N <: NAT] <: U, IfZero <: U, U] <: U
  
  // Compare
  type ? [N <: NAT] <: CMP
  
  // FoldR
  type :^ [Init <: Type, Type, F <: FOLD[NAT, Type]] <: Type
  // FoldL
  //type ^: [Init <: Type, Type, F <: FOLD]
}
object NAT {

  type ? [A <: NAT, B <: NAT] = A# ? [B]
  
  sealed trait _0 extends NAT {
    type ~[NonZero[N <: NAT] <: U, IfZero <: U, U] = IfZero
    
    type ?[N <: NAT] = N# ~ [LT, EQ, CMP]
    type LT[_] = CMP.LT
    
    type :^ [Init <: Type, Type, F <: FOLD[NAT, Type]] = Init
  }
  
  sealed trait ++ [N <: NAT] extends NAT {
    type ~[NonZero[N <: NAT] <: U, IfZero <: U, U] = NonZero[N]
    
    type ?[That <: NAT] = That# ~ [N# ?, GT, CMP]
    
    type :^ [Init <: Type, Type, F <: FOLD[NAT, Type]] =
      F# \ [++[N], N# :^ [Init, Type, F]]
  }
  
  type + [A <: NAT, B <: NAT] = A# :^ [B, NAT, `+1`]
  type `+1` = FOLD[NAT, NAT] {
    type \ [N <: NAT, Acc <: NAT] = ++[Acc]
  }
  
  type * [A <: NAT, B <: NAT] = A# :^ [_0, NAT, Σ[B]]
  type Σ [By <: NAT] = FOLD[NAT, NAT] {
    type \ [N <: NAT, Acc <: NAT] = By + Acc
  }
  
  type ** [A <: NAT, B <: NAT] = A# :^ [_1, NAT, E[B]]
  type E [By <: NAT] = FOLD[NAT, NAT] {
    type \ [N <: NAT, Acc <: NAT] = *[By, Acc]
  }
  
  type % [A <: NAT, B <: NAT] = A# :^ [_0, NAT, M[B]]
  type M [By <: NAT] = FOLD[NAT, NAT] {
    type \ [N <: NAT, Acc <: NAT] = Wrap[++[Acc]]#If[_0, ++[Acc], NAT]
    type Wrap[Acc <: NAT] = By# ?[Acc]# ==
  }
  
  type == [A <: NAT, B <: NAT] = (A ? B)# ==
  
  type _1 = ++[_0]
  type _2 = ++[_1]
  type _3 = ++[_2]
  type _4 = ++[_3]
  type _5 = ++[_4]
  type _6 = ++[_5]
  type _7 = ++[_6]
  type _8 = ++[_7]
  type _9 = ++[_8]
  type _10 = ++[_9]
  
  type NAT_[N <: NAT] = Manifester[N, Int]
  implicit object _0_ extends NAT_[_0](0)
  implicit def ++[N <: NAT](implicit n : NAT_[N]) = new NAT_[++[N]](1 + n.value)
  
  def __[N <: NAT](implicit n: NAT_[N]) = n.value
}