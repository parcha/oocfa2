package com.android.dx.cfa2

import java.io._

abstract class Logger {
  def log(s:String) : Unit = stream println s
  final def apply(s: String) = log(s)
  def finish(): Unit = stream.close()
  // HACK: used to access underlying printstream for e.g. stack traces
  val stream: PrintStream
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

class PrintLogger(val stream:PrintStream) extends Logger
class FileLogger(f:File) extends Logger {
  def this(name:String) = this(new File(name))
  val stream = new PrintStream(f)
  override def log(s:String) = 
    try stream println s
    catch {
      case e:IOException => e printStackTrace
      case e:Exception => throw e
    }
}
class CompressedFileLogger(f:File) extends FileLogger(f) {
  def this(name:String) = this(new File(name+".gz"))
  import java.util.zip._
  override val stream =
    new PrintStream(new GZIPOutputStream(new FileOutputStream(f)))
}
class NullaryLogger extends Logger {
  override def log(s:String) = {}
  override def finish() = Unit
  val stream = null
}
class ConditionalLogger(cond: =>Boolean, l: Logger) extends Logger {
  override def log(s: String) = if(cond) l log s
  override def finish() = l.finish()
  val stream = l.stream
}
class DualLogger(l1: Logger, l2: Logger) extends Logger {
  override def log(s: String) = {
    l1 log s
    l2 log s
  }
  val stream = l1.stream
  override def finish() = {l1.finish(); l2.finish()}
}