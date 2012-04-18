package jp.kobe_u.copris

import scala.collection._

/**
 * Case class for solutions
 */
case class Solution(intValues: Map[Var,Int], boolValues: Map[Bool,Boolean]) {
  /** Returns the value of the given term */
  def apply(x: Term): Int = x match {
    case v: Var => intValues(v)
    case _ => x.value(this)
  }
  /** Returns the values of the given terms */
  def apply(x0: Term, x1: Term, xs: Term*): Seq[Int] =
    (x0 +: x1 +: xs).map(apply(_))
  /** Returns the value of the given constraint */
  def apply(c: Constraint): Boolean = c match {
    case p: Bool => boolValues(p)
    case _ => c.value(this)
  }
  /** Returns the values of the given constraints */
  def apply(c0: Constraint, c1: Constraint, cs: Constraint*): Seq[Boolean] =
    (c0 +: c1 +: cs).map(apply(_))
}

/**
 * Object Timer (experimental)
 */
object Timer {
  var count = 0
}
/**
 * Class Timer (experimental)
 */
class Timer(val timeout: Long) {
  val deadline: Long =
    if (timeout > 0) currentTime + timeout else 0
  def currentTime: Long =
    scala.compat.Platform.currentTime
  def restTime: Long =
    if (deadline > 0) deadline - currentTime else Long.MaxValue
  var timerThread: Thread = null
  var mainThread: Thread = null
  var timeoutTask: () => Unit = null
  var count = {Timer.count = Timer.count + 1; Timer.count}
  println("Timer " + count + " new")
  def setTimeoutTask(task: => Unit) = {
    timeoutTask = () => task
  }
  def start = {
    mainThread = Thread.currentThread
    timerThread = null
    if (deadline > 0) {
      timerThread = new Thread() {
        override def run = {
          println("Timer " + count + " start " + timeout)
          while (timerThread != null && currentTime < deadline) {
            Thread.sleep(10)
          }
          if (timerThread != null) {
            timerThread = null
            // Interrupt
            println("Timer " + count + " interrupt")
	    if (timeoutTask != null) {
	      timeoutTask()
	      timeoutTask = null
	    }
            mainThread.interrupt
          } else {
            // Done already
          }
          println("Timer " + count + " end")
        }
      }
      timerThread.start
    }
  }
  def stop = {
    println("Timer " + count + " stop")
    timeoutTask = null
    timerThread = null
  }
  def raiseTimeout = {
    println("Timer " + count + " timeout")
    if (timeoutTask != null) {
      timeoutTask()
      stop
    }
    throw new InterruptedException("Timeout (" + timeout + ") exceeded")
  }
  def checkTimeout = {
    if (restTime <= 0)
      raiseTimeout
  }
}

/**
 * Trait for CSP solvers
 */
trait SolverTrait {
  /** Initializes the solver */
  def init: Unit
  /** Finds the first solution */
  def find: Boolean
  /** Finds the next solution */
  def findNext: Boolean
  /** Finds the optimum solution */
  def findOpt: Boolean
  /** Returns the current solution */
  def solution: Solution

  /** Returns the integer variable value of the current solution */
  @deprecated("use apply method of [[jp.kobe_u.copris.Solution]] instead", "1.0.1")
  def value(x: Var): Int = solution.intValues(x)
  /** Returns the Boolean variable value of the current solution */
  @deprecated("use apply method of [[jp.kobe_u.copris.Solution]] instead", "1.0.1")
  def value(p: Bool): Boolean = solution.boolValues(p)
  /** Returns the integer variable values of the current solution */
  @deprecated("use apply method of [[jp.kobe_u.copris.Solution]] instead", "1.0.1")
  def values(x: Var, xs: Var*): Seq[Int] =
    value(x) +: xs.map(value(_))
  /** Returns the Boolean variable values of the current solution */
  @deprecated("use apply method of [[jp.kobe_u.copris.Solution]] instead", "1.0.1")
  def values(p: Bool, ps: Bool*): Seq[Boolean] =
    value(p) +: ps.map(value(_))
}

/**
 * Abstract class for CSP solvers
 */
abstract class AbstractSolver(csp: CSP) extends SolverTrait {
  /** Options of the solver */
  var options: Map[String,String] = Map.empty
  /** Timeout value in miliseconds (experimental) */
  var timeout: Long = 0
  /** Timer (experimental) */
  var timer: Timer = null
  /** Starts the timer (experimental) */
  def startTimer(t: Long) = {
    if (t > 0) {
      timeout = t
      timer = new Timer(timeout)
      timer.start
    }
  }
  /** Stops the timer (experimental) */
  def stopTimer = {
    if (timer != null)
      timer.stop
    timer = null
  }
  /** Checks the timeout (experimental) */
  def checkTimeout = {
    if (timer != null)
      timer.checkTimeout
  }
  /** Specifies the clean-up tasks of the timeout (experimental) */
  def setTimeoutTask(task: => Unit) = {
    if (timer != null)
      timer.setTimeoutTask(task)
  }
  /** Raises the interrupted exception for timeout (experimental) */
  def raiseTimeout = {
    if (timer != null)
      timer.raiseTimeout
    throw new InterruptedException("Timeout (" + timeout + ") exceeded")
  }

  /** Status of the solver (experimental) */
  var solverStats: Seq[Map[String,Map[String,Number]]] =
    Seq(Map.empty)
  /** Shifts the status (experimental) */
  def shiftSolverStats =
    solverStats = solverStats :+ Map.empty
  /** Gets the last status of ~name~ (experimental) */
  def getSolverStat(name: String): Map[String,Number] =
    if (solverStats.last.contains(name))
      solverStats.last(name)
    else
      Map.empty
  /** Sets the current status of `name` (experimental) */
  def addSolverStat(name: String, stat: Map[String,Number]): Unit = {
    val stat1 = getSolverStat(name) ++ stat
    solverStats = solverStats.init :+ (solverStats.last + (name -> stat1))
  }
  /** Adds the current status of `name` (experimental) */
  def addSolverStat(name: String, key: String, value: Number): Unit =
    addSolverStat(name, Map(key -> value))
  /** Measures the time spent for executing `block` (experimental) */
  def measureTime[T](name: String, key: String)(block: => T) = {
    try {
      val time0 = scala.compat.Platform.currentTime
      val value = block
      Thread.sleep(1)
      val time1 = scala.compat.Platform.currentTime
      addSolverStat(name, key, time1 - time0)
      value
    } catch {
      case e: InterruptedException => {
        println(e)
        // e.printStackTrace
        raiseTimeout
      }
      case e: java.nio.channels.ClosedByInterruptException => {
        // e.printStackTrace
        println(e)
        raiseTimeout
      }
    }
  }

  /** Body of the `find` method */
  def findBody: Boolean
  /** Body of the `findNext` method */
  def findNextBody: Boolean
  /** Body of the `findOpt` method */
  def findOptBody: Boolean
  /** Body of the `findOptBound` method */
  def findOptBoundBody(lb: Int, ub: Int): Boolean
  /* */
  def find = {
    addSolverStat("csp", "variables", csp.variables.size)
    addSolverStat("csp", "bools", csp.bools.size)
    addSolverStat("csp", "constraints", csp.constraints.size)
    measureTime("time", "find") {
      init
      val result = findBody
      addSolverStat("result", "find", if (result) 1 else 0)
      result
    }
  }
  /* */
  def findNext = {
    shiftSolverStats
    measureTime("time", "findNext") {
      val result = findNextBody
      addSolverStat("result", "find", if (result) 1 else 0)
      result
    }
  }
  /* */
  def findOpt = {
    addSolverStat("csp", "variables", csp.variables.size)
    addSolverStat("csp", "bools", csp.bools.size)
    addSolverStat("csp", "constraints", csp.constraints.size)
    measureTime("time", "findOpt") {
      init
      val result = findOptBody
      shiftSolverStats
      addSolverStat("result", "find", if (result) 1 else 0)
      if (result && (csp.isMinimize || csp.isMaximize)) {
        addSolverStat("result", "objective", solution(csp.objective))
      }
      result
    }
  }
  /** Finds a solution within the given bounds */
  def findOptBound(lb: Int, ub: Int) = {
    shiftSolverStats
    addSolverStat("csp", "lb", lb)
    addSolverStat("csp", "ub", ub)
    measureTime("time", "findOptBound") {
      val result = findOptBoundBody(lb, ub)
      addSolverStat("result", "find", if (result) 1 else 0)
      result
    }
  }
}

/**
 * Factory for default solver.
 *
 * [[jp.kobe_u.copris.sugar.Solver]] is returned as the default solver.
 * @see [[jp.kobe_u.copris.sugar.Solver]]
 */
object DefaultSolver {
  def apply(csp: CSP): AbstractSolver =
    new sugar.Solver(csp)
}
