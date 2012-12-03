package com.android.dx.cfa2.`val`

import Type._

sealed abstract class RetVal extends Immutable with NotNull with Serializable {
  import RetVal._
  def union[RV <: RetVal](other: RV) : RetVal
}
object RetVal {
  trait WithReturn[T <: Returnable] extends RetVal { val rv: Val[T] }
  type WithReturn_ = WithReturn[_ <: Returnable]
  final case class Return [T <: Returnable] (rv:Val[T]) extends RetVal with WithReturn[T] {
    def union[RV <: RetVal](other: RV) = other match {
      case Return(v)     =>
        if(v == rv) this
        else Return(rv.union(v).asInstanceOf[Val[Returnable]])
      case Throw(ev) => Dual(rv, ev)
      case d:Dual_  => d union this
    }
  }
  type Return_ = Return[_ <: Returnable]
  
  trait WithThrow[E <: Exceptional] extends RetVal { val ev: Val[E] }
  type WithThrow_ = WithThrow[_ <: Exceptional]
  final case class Throw [E <: Exceptional] (ev:Val[E]) extends RetVal with WithThrow[E] {
    def union[RV <: RetVal](other: RV) = other match {
      case Return(rv)       => Dual(rv, ev)
      case Throw(v) =>
        if(v == ev) this
        else Throw(ev.union(v).asInstanceOf[Val[Exceptional]])
      case d:Dual_   => d union this
    }
  }
  type Throw_ = Throw[_ <: Exceptional]
  
  final case class Dual[T <: Returnable, E <: Exceptional] private[RetVal]
                       (rv: Val[T], ev: Val[E]) extends RetVal with WithReturn[T] with WithThrow[E] {
    def union[RV <: RetVal](other: RV) = other match {
      case Return(v)      => Dual(rv union v, ev)
      case Throw(v) => Dual(rv, (ev union v).asInstanceOf[Val[Exceptional]])
      case Dual(rv_, ev_) => Dual(rv union rv_, (ev union ev_).asInstanceOf[Val[Exceptional]])
    }
  }
  type Dual_ = Dual[_ <: Returnable, _ <: Exceptional]
}