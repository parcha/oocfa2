package com.android.dx.cfa2.env

import com.android.dx
import dx.cfa2
import cfa2._
import `var`._
import `val`._

import scala.collection._

final class TraceFrame
(final val self: TraceFrame.M = new immutable.HashMap[Var.Register_, Val_])
extends immutable.MapProxy[Var.Register_, Val_]
with Env[Var.Register_] with TraceFrame.ProxyFactoried {
  def union(that: TraceFrame) = TraceFrame.union(this, that)
  override def toString = self.toString
  override def equals(that:Any) = self equals that
}
object TraceFrame extends EnvFactory[Var.Register_, TraceFrame](new TraceFrame(_))