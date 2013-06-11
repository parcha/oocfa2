package com.android.dx.cfa2.analysis

import com.android.dx
import dx.cfa2
import cfa2._

import dx.opt.Optimizer

import cfa2.wrap._

import scala.collection._

import java.lang.reflect.{Method => JMethod}

object Analysis {
  abstract class Opts protected[Analysis] extends Immutable with NotNull {
    import Opts._
    def single_threaded: Boolean
    def debug: Boolean
    // In seconds
    def timeout: Int
    def outPath: String
    
    def continueOnOverflow = !debug
    def continueOnInternalError = !debug
    def arrayCovarianceBehavior: ArrayCovarianceBehavior = ArrayCovarianceBehaviors.Warn
    
    lazy val extra_classpaths: Option[List[java.net.URL]] = None
    protected[analysis] lazy val starting_points: Iterable[MethodIDer] = immutable.Seq(MethodIDer.Accessible)
    
    protected[this] def mkLog(sym: Symbol): (Symbol, Logger) = (sym, new FileLogger(outPath+"/"+sym.name+".log"))
    protected[this] def mkLog(sym: Symbol, primary: Logger): (Symbol, Logger) =
      (sym, new DualLogger(primary, mkLog(sym)._2))
    
    private[this] lazy val debugLog = new ConditionalLogger(debug, new CompressedFileLogger(outPath+"/debug.log"))
    protected[this] def _logs = immutable.Map(
      // FIXME: Hack to get other streams interleaved into debug
      mkLog('out, new DualLogger(new PrintLogger(System.out), debugLog)),
      mkLog('info, new DualLogger(new PrintLogger(System.err), debugLog)),
      mkLog('warn, new DualLogger(new PrintLogger(System.err), debugLog)),
      ('debug, debugLog),
      mkLog('error, new PrintLogger(System.err)))
    final lazy val logs = _logs
    lazy val log = new Opts.Loggers(logs)
    def loggers = log.logs.values
  }
  object Opts {
    object ArrayCovarianceBehaviors extends Enumeration {
      val Warn, /** Warn and keep going assuming everything is fine **/
          Mask /** Mask it with an unknown value of the correct type **/
          = Value
    }
    type ArrayCovarianceBehavior = ArrayCovarianceBehaviors.Value
    
    final class Loggers(val logs: Map[Symbol, Logger]) {
      def apply(l: Symbol) = logs(l)
    }
  }
  
  final class UnimplementedOperationException(msg: String) extends Exception(msg)
  
  lazy val singleton = {assert(_singleton != null); _singleton}
  private[Analysis] var _singleton : Analysis[Opts] = null
  
  // Global, hackish convenience
  def log(sym:Symbol)(s:String) = singleton.opts.log(sym)(s)
  def logs = singleton.opts.logs
  def debug = singleton.opts.debug
}

import Analysis._
abstract class Analysis[+O<:Opts](protected[cfa2] final val opts:O) extends Optimizer {
	protected[cfa2] val methods: GenSet[DalvikMethod]
	protected[cfa2] val reflMethodMap: GenMap[JMethod, DalvikMethod]
	protected[cfa2] val classes: GenSet[DalvikClass]
	protected[cfa2] val ifieldMap: GenMap[IFieldSpec, FieldSlot.Known]
	protected[cfa2] val sfieldMap: GenMap[SFieldSpec, FieldSlot]
}