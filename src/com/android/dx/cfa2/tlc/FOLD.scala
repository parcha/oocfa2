package com.android.dx.cfa2.tlc

trait FOLD[-ELEM, VALUE] {
  type \ [E <: ELEM, Acc <: VALUE] <: VALUE
  def apply[E <: ELEM, Acc <: VALUE](e: E, acc: Acc): (E \ Acc)
}