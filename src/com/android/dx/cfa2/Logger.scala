package com.android.dx.cfa2

import java.io._

abstract class Logger extends scala.util.logging.Logged {
  def log(s:String) : Unit
  final def apply(s: String) = log(s)
}
object Logger {
  type Log = Any
  def apply(l:Log) = l match {
    case ps:PrintStream => new PrintLogger(ps)
    case f:File => new FileLogger(f)
    case s:String => new FileLogger(s)
    case null => new NullaryLogger
  }
}

class PrintLogger(ps:PrintStream) extends Logger {
  override def log(s:String) = ps println s
}
class FileLogger(f:File) extends Logger {
  def this(name:String) = this(new File(name))
  protected[this] val ps = new PrintStream(f)
  override def log(s:String) = 
    try ps println s
    catch {
      case e:IOException => e printStackTrace
      case e:Exception => throw e
    }
}
class CompressedFileLogger(f:File) extends FileLogger(f) {
  def this(name:String) = this(new File(name+".gz"))
  import java.util.zip._
  override protected[this] val ps =
    new PrintStream(new GZIPOutputStream(new FileOutputStream(f)))
}
class NullaryLogger extends Logger {
  override def log(s:String) = {}
}
class ConditionalLogger(cond: =>Boolean, l: Logger) extends Logger {
  override def log(s: String) = if(cond) l log s
}
class DualLogger(l1: Logger, l2: Logger) extends Logger {
  override def log(s: String) = {
    l1 log s
    l2 log s
  }
}