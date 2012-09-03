package com.android.dx.cfa2.test

import com.android.dx.opt.{OptimizationContext => Context, _}
import com.android.dx.cfa2.CFA2Analysis

object CFA2Factory extends OptimizerFactory[CFA2Analysis] {

  val getOptimizerName = "CFA2"
  def create(ctxt: Context, args: Array[String]) =
    new CFA2Analysis(ctxt, CFA2Analysis.defaultOpts)
  def create(ctxt: Context) = create(ctxt, null)
}