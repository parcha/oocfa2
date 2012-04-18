package jp.kobe_u.copris

/**
 * Trait for Copris DSL which provides methods for CSP and CSP solver.
 * This trait also provides implicit conversion of converting
 * scala Symbols to CSP integer variables ([[jp.kobe_u.copris.Var]]).
 */
trait CoprisTrait extends CSPTrait with SolverTrait {
  /** Implicit conversion from scala Symbol to [[jp.kobe_u.copris.Var]]. */
  implicit def symbol2var(s: Symbol) = Var(s.name)
  /** CSP to be used */
  def csp: CSP
  /** Solver to be used */
  def solver: AbstractSolver
  /** Changes the solver to be used */
  def use(newSolver: AbstractSolver): Unit
  /** Sets the options of the solver */
  def options(opts: Map[String,String]): Unit =
    solver.options = opts
  /** Initializes the CSP and solver */
  def init: Unit = { csp.init; solver.init }
  /* */
  def int(x: Var, d: Domain) = csp.int(x, d)
  /* */
  def bool(p: Bool) = csp.bool(p)
  /* */
  def add(cs: Constraint*) = csp.add(cs: _*)
  /* */
  def minimize(x: Var) = csp.minimize(x)
  /* */
  def maximize(x: Var) = csp.maximize(x)
  /* */
  def satisfiedBy(solution: Solution) = csp.satisfiedBy(solution)
  /* */
  def find = solver.find
  /* */
  def findNext = solver.findNext
  /* */
  def findOpt = solver.findOpt
  /** Shows the CSP */
  def show = print(csp.output)
  /** Shows the current solution */
  def solution = solver.solution
  /** Starts the timer (experimental) */
  def startTimer(timeout: Long) = solver.startTimer(timeout)
  /** Stops the timer (experimental) */
  def stopTimer = solver.stopTimer
  /** Returns the status of the solver (experimental) */
  def stats = solver.solverStats
}

/**
 * Class for Copris DSL which provides methods for CSP and CSP solver.
 * @constructor Constructs Copris with the given CSP and solver
 */
class Copris(val csp: CSP, var solver: AbstractSolver) extends CoprisTrait {
  /** Constructs Copris with the given CSP and [[jp.kobe_u.copris.DefaultSolver]] */
  def this(csp: CSP) =
    this(csp, DefaultSolver(csp))
  /** Constructs Copris with empty CSP and [[jp.kobe_u.copris.DefaultSolver]] */
  def this() =
    this(CSP())
  /** Changes the solver to be used */
  def use(newSolver: AbstractSolver): Unit =
    solver = newSolver
}

/**
 * Object for Copris DSL which provides methods for CSP and CSP solver.
 */
object dsl extends CoprisTrait {
  /** Dynamic variable of Copris */
  val coprisVar = new util.DynamicVariable[Copris](new Copris())
  /** Returns Copris object */
  def copris = coprisVar.value
  /** Returns CSP */
  def csp = coprisVar.value.csp
  /** Returns CSP solver */
  def solver = coprisVar.value.solver
  /* */
  def use(newSolver: AbstractSolver) =
    coprisVar.value.use(newSolver)
  /** Executes the `block` under the specified Copris */
  def using(copris: Copris = new Copris())(block: => Unit) =
    coprisVar.withValue(copris) { block }
}
