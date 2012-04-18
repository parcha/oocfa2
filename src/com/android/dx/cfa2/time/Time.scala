package com.android.dx.cfa2.time

abstract class Time private[time] () {
    def cmp(t: Time) : Time.CMP.Value
    import Time.CMP._
    final def >(t: Time) = cmp(t) == GT
    final def <(t: Time) = cmp(t) == LT
    final def ==(t: Time) = cmp(t) == EQ
    final def >=(t: Time) = {val c = cmp(t); c == GT || c == EQ}
    final def <=(t: Time) = {val c = cmp(t); c == LT || c == EQ}
}
object Time {
  object CMP extends Enumeration {
    val LT, GT, EQ, NC = Value
    def invert(cmp: Value) = cmp match {
      case LT => GT
      case GT => LT
      case _  => cmp
    }
  }
}

trait Timekeeper {
  type Timestamp <: Time
  val timestamp : Timestamp
}