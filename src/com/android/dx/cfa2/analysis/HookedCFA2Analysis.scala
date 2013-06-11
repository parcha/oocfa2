package com.android.dx.cfa2.analysis

import com.android.dx
import dx.cfa2._
import collection._
import `val`._

object HookedCFA2Analysis {
  type Hook[-Args, +Ret] = Args => Option[Ret]  
  type InstanceHook = Hook[(Instantiable, Val_, IParams),
                           IParams]
  type CloneHook = Hook[(Instantiable#Instance, Val_, IParams),
                        IParams]
  type UnknownMethodHook = Hook[(wrap.MethodDesc, Seq[Val_]),
                                Val_]
  type KnownMethodHook = Hook[(wrap.DalvikMethod, Seq[Val_], CFA2Analysis.FSummary),
                              CFA2Analysis.FSummary]
  
  def HOOK[Args, Ret, H <: Hook[Args, Ret]]
          (hooks: Iterable[H], args: Args, act:Ret=>Unit) =
    for(hook <- hooks) hook(args) match {
      case None      =>
      case Some(ret) => act(ret)
    }
  
  trait Opts extends CFA2Analysis.Opts {
    // Hooks
    val instance_hooks: Iterable[InstanceHook] = immutable.Seq()
    val clone_hooks: Iterable[CloneHook] = immutable.Seq()
    val umethod_hooks: Iterable[UnknownMethodHook] = immutable.Seq()
    val kmethod_hooks: Iterable[KnownMethodHook] = immutable.Seq()
  }
}
trait HookedCFA2Analysis[+O<:HookedCFA2Analysis.Opts] extends CFA2Analysis[O] {
  protected[cfa2] lazy val instance_hooks = opts.instance_hooks
  protected[cfa2] lazy val clone_hooks = opts.clone_hooks
  protected[this] lazy val umethod_hooks = opts.umethod_hooks
  protected[this] lazy val kmethod_hooks = opts.kmethod_hooks
}