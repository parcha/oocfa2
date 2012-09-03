package com.android.dx.cfa2.test

import com.android.dx
import scala.collection._
import com.android.dx.command.dexer
import com.android.dx.cfa2.test.CFA2Factory

object CFA2Test {
  def main(args: Array[String]) = {
    import dx.command.dexer
    dexer.Main.devtestOptimizerFactory = CFA2Factory
    dexer.Main.main(args)
  }
}