package com.android.dx.cfa2.env

import com.android.dx
import dx.cfa2
import cfa2._
import `var`._
import `val`._

import scala.collection._

/**
 * Keeps an "update list" of raw references which have been updated, along with their
 * current values. In this way, immutable references can simply be implicitly handled
 * via first-class references instead of having to be looked up.
 */
final class HeapEnv
(final val self: HeapEnv.M = HeapEnv.defaultM,
 private[this] final val updateHook: (Var.RawHeap_, Val_)=>Unit = null)
extends Env[Var.RawHeap_] with immutable.MapProxy[Var.RawHeap_, Val_] with HeapEnv.ProxyFactoried {
  // TODO: Updater... How should this actually work? Should we merge vals?
  // Current thinking is shouldn't merge because we're tracking the reference itself
  def update(`var`:Var.RawHeap_, `val`:Val_) =
    if(updateHook != null) updateHook(`var`, `val`)
}
object HeapEnv extends EnvFactory[Var.RawHeap_, HeapEnv](new HeapEnv(_, null)) {
  val defaultM = immutable.HashMap.empty[Var.RawHeap_, Val_]
}