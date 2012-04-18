package com.android.dx.cfa2.tlc

sealed trait BOOL {
  type If[T <: U, F <: U, U] <: U
}
object BOOL {

  sealed trait True extends BOOL {
    type If[T <: U, F <: U, U] = T
  }
  sealed trait False extends BOOL {
    type If[T <: U, F <: U, U] = F
  }
  
  type && [A <: BOOL, B <: BOOL] = A#If[B, False, BOOL]
  type || [A <: BOOL, B <: BOOL] = A#If[True, B, BOOL]
  type -  [B <: BOOL] = B#If[False, True, BOOL]
  
  type BOOL_[B <: BOOL] = Manifester[BOOL, Boolean]
  implicit object false_ extends BOOL_[False](false)
  implicit object true_ extends BOOL_[True](true)
  
  implicit def __[B <: BOOL](implicit b: BOOL_[B]): Boolean = b.value
  
}