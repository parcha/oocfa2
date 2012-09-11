package com.android.dx.cfa2

import collection._

object HookedCFA2Analysis {
  trait Opts extends CFA2Analysis.Opts {
    // Hooks
    val instance_hooks: Iterable[InstanceHook] = immutable.Seq()
    val clone_hooks: Iterable[CloneHook] = immutable.Seq()
    val umethod_hooks: Iterable[UnknownMethodHook] = immutable.Seq()
    val kmethod_hooks: Iterable[KnownMethodHook] = immutable.Seq()
  }
}
trait HookedCFA2Analysis[+O<:HookedCFA2Analysis.Opts] extends CFA2Analysis[O] {
  protected[cfa2] val instance_hooks = opts.instance_hooks
  protected[cfa2] val clone_hooks = opts.clone_hooks
  protected[this] val umethod_hooks = opts.umethod_hooks
  protected[this] val kmethod_hooks = opts.kmethod_hooks
}