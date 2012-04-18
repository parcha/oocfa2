package com.android.dx.cfa2.time

/**
 *  For immutable timekeeping
 */
class UnitTime extends Time with Immutable {
  import Time.CMP._
  def cmp(t: Time) = t match {
    case t_ :UnitTime => if(this eq t) EQ else NC
    case _            => NC
  }
}