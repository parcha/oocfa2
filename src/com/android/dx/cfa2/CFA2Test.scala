package com.android.dx.cfa2

import com.android.dx
import scala.collection._

object CFA2Test {
  def main(args: Array[String]) = {
    import dx.command.dexer
    dexer.Main.devtestOptimizerFactory = CFA2Factory
    dexer.Main.main(args)
    REQBREAK
  }
}