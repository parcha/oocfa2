package com.android.dx.cfa2.time

/**
 *  For mutable timekeeping
 */
class LinearTime extends Time {
  protected var i:Int = 0
  def ++ = i+=1
  import Time.CMP._
  def cmp(t: Time) = t match {
    case t_ :LinearTime => (i - t_.i).signum match {
      case -1 => LT
      case 0  => EQ
      case 1  => GT 
    }
    case _              => NC
  }
}