package com.android.dx.cfa2

import com.android.dx
import dx.opt.{OptimizationContext => Context, _}

object CFA2Factory extends OptimizerFactory[CFA2Analysis] {

  val getOptimizerName = "CFA2"
  def create(ctxt: Context, args: Array[String]) =
    new CFA2Analysis(ctxt, CFA2Analysis.defaultOpts)
  def create(ctxt: Context) = create(ctxt, null)
}