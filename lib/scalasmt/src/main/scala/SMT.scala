package cap.scalasmt

import Zeros._
import Debug._

/* 
 * Translator to SMT-LIB2.
 * @author kuat
 */
object UnsatException extends RuntimeException("inconsistent model")

case class SolverException(msg: String) extends RuntimeException(msg)

trait Solver {
  /** Issue a command and expect success. */
  def command(s: String) {>(s);
    val reply = <();
    if (reply != "success") 
      throw SolverException("unexpected reply: " + reply)
  }
  /** Check satisfiability. */
  def check(): Boolean = {>("(check-sat)"); <() == "sat"}
  /** Check for another model. */
  def next(): Boolean = {>("(next-sat)"); <() == "sat"}
  /** Retrieve the model. */
  def model(): String = {>("(get-info model)"); <<().mkString}
  /** Assert a boolean condition. */
  def assert(s: String) = command("(assert " + s + ")")
  /** Push a context */
  def push() = command("(push)")
  /** Pop a context */
  def pop() = command("(pop)")
  /** Terminate the solver. */
  def close()
  /** Send to solver. */ 
  protected def >(s: String)
  /** Receive a line from solver. */
  protected def <(): String
  /** Receive all available lines from solver. */
  protected def <<(): List[String]
}

trait Logging extends Solver {
  debug("Logging solver: " + this);
  abstract override def >(s: String) {debug(s); super.>(s)}
}

class Z3 extends Solver {
  import java.io._

  var TIMEOUT = 10;
  
  private def PATH = Option(System.getProperty("smt.home")) match {
    case Some(path) => path
    case None => System.getProperty("user.home") + "/opt/z3/bin/z3"
  }

  private def COMMANDS ="-smt2" :: "-m" :: "-t:" + TIMEOUT :: "-in" :: Nil
  
  private var process = {
    val pb = new ProcessBuilder((PATH :: COMMANDS).toArray: _*);
    pb.redirectErrorStream(true);
    pb.start;
  }
  
  private var input = new BufferedWriter(new OutputStreamWriter(process.getOutputStream));
  private var output = new BufferedReader(new InputStreamReader(process.getInputStream));
  
  protected def >(s: String) = {input.write(s); input.newLine; input.flush}
  protected def <() = output.readLine 

  command("""(set-logic QF_NIA)""")
  command("""(set-option set-param "ELIM_QUANTIFIERS" "true")""")

  override def close {input.close; output.close; process.destroy;}

  protected def <<() = {
    import scala.collection.mutable
    val out = new mutable.ListBuffer[String]
    var line = <();
    while (line != null) {
      out += line;
      if (output.ready) line = <() else line = null;
    }
    out.toList;
  }
}
 
/**
 * Expression translators.
 */ 
object SMT {
  private def variable(v: Var[_])(implicit env: Environment) =
    if (env.has(v))
      env(v).toString
    else
      v.toString

  private def formula(f: Expr[Boolean])(implicit env: Environment, sc: Scope): String = f match {
    case And(a,b) => "(and " + formula(a) + " " + formula(b) + ")"
    case Or(a,b) => "(or " + formula(a) + " " + formula(b) + ")"
    case Not(a) => "(not " + formula(a) + ")"
    case BoolEq(a,b) => "(= " + formula(a) + " " + formula(b) + ")"
    case BoolVal(v) => v.toString
    case IntEq(a,b) => "(= " + integer(a) + " " + integer(b) + ")"
    case Leq(a,b) => "(<= " + integer(a) + " " + integer(b) + ")"
    case Geq(a,b) => "(>= " + integer(a) + " " + integer(b) + ")" 
    case LT(a,b) => "(< " + integer(a) + " " + integer(b) + ")"
    case GT(a,b) => "(> " + integer(a) + " " + integer(b) + ")"  
    case BoolConditional(c,a,b) => "(if " + formula(c) + " " + 
      formula(a) + " " + formula(b) + ")" 
    case ObjectEq(a,b) => "(= " +  atom(a) + " " + atom(b) + ")"
    case RelEq(a,b) => "(forall (x Object) (iff " + 
      set(a)("x", env, sc) + " " + set(b)("x", env, sc) + "))"
    case RelSub(a,b) => "(forall (x Object) (=> " + 
      set(a)("x", env, sc) + " " + set(b)("x", env, sc) + "))"
    case v: BoolVar => variable(v)
  }

  private def integer(e: Expr[BigInt])(implicit env: Environment, sc: Scope): String = e match {
    case Plus(a,b) => "(+ " + integer(a) + " " + integer(b) + ")"
    case Minus(a,b) => "(- " + integer(a) + " " + integer(b) + ")"
    case Times(a,b) => "(* " + integer(a) + " " + integer(b) + ")"
    case IntConditional(c,a,b) => "(if " + formula(c) + " " + integer(a) + " " + integer(b) + ")"
    case IntVal(i) => i.toString
    case ObjectIntField(root, f) => "(" + f + " " + atom(root) + ")"
    case v: IntVar => variable(v) 
  }

  private def atom(e: Expr[Atom])(implicit env: Environment, sc: Scope): String = e match {
    case ObjectConditional(cond, thn, els) => "(if " + formula(cond) + " " + atom(thn) + " " + atom(els) + ")"
    case Object(o) => sc.encode(o)
    case ObjectField(root, f) => "(" + f + " " + atom(root) + ")"
    case v: ObjectVar[_] =>  
      if (env.has(v)) 
        sc.encode(env(v))
      else
        v.toString
  }

  private def set(e: Expr[Set[Atom]])(implicit q: String, env: Environment, sc: Scope): String = e match {
    case Union(a,b) => "(or " + set(a) + " " + set(b) + ")"
    case Diff(a,b) => "(and " + set(a) + " (not " + set(b) + "))"
    case Intersect(a,b) => "(and " + set(a) + " " + set(b) + ")"
    case Singleton(o) => "(= " + q + " " + atom(o) + ")"
    case ObjectSet(os) => if (os.size == 0) "false" else "(or " + os.map("(= " + q + " " + sc.encode(_) + ")").mkString(" ") + ")"
    case RelJoin(root, f) => 
      val r = q + "0";
      "(exists (" + r + " Object) (and (= " + q + 
        " (" + f + " " + r + ")) " + set(root)(r, env, sc) + "))"
    case v: ObjectSetVar => 
      if (env.has(v))
        set(ObjectSet(env(v)))
      else
        "(" + v + " " + q + ")"
  }

  /** 
   * Bounded universe of atoms (and their fields)   
   */

  private object Scope {
    implicit def fromField(f: FieldDesc[_]) = Scope(fields = Set(f))
    implicit def fromVar(v: Var[_]) = Scope(vars = Set(v))
    implicit def fromAtom(o: Atom) = Scope(objects = Set(o))
    implicit def fromAtomSet(o: Set[Atom]) = Scope(objects = o)
  }

  private case class Scope(objects: Set[Atom] = Set(), 
                       fields: Set[FieldDesc[_]] = Set(), 
                       vars: Set[Var[_]] = Set()) {
    def ++ (that: Scope) = Scope(
      this.objects ++ that.objects, 
      this.fields ++ that.fields, 
      this.vars ++ that.vars
    )

    lazy val uniq = objects.toList
    def encode(o: Atom): String = 
      if (o == null) 
        "null"
      else {
        assert(uniq.contains(o))
        "o" + uniq.indexOf(o)
      }
    def decode(s: String): Atom = 
      if (s == "null")
        null
      else 
        uniq(s.substring(1).toInt)
    
    def classId(klas: Class[_]): String = 
      if (klas == null) "Null" else klas.getName.replace(".","").replace("$","")
   
    def atomId(o: Atom): String = 
      if (o == null) classId(null) else classId(o.getClass)

    def size = objects.size + fields.size + vars.size
    override def toString = objects.size + " objects; " + fields.size + " fields; " +  vars.size + " vars"
  }
 
  private def univ(f: Expr[_])(implicit env: Environment): Scope = (f: @unchecked) match {
    case f: BinaryExpr[_] => univ(f.left) ++ univ(f.right)
    case f: Ite[_] => univ(f.cond) ++ univ(f.thn) ++ univ(f.els)
    case f: UnaryExpr[_] => univ(f.sub)
    case v: Var[_] => v
    case os: ObjectSet => os.eval
    case o: Object[_] => o.eval
    case RelJoin(root, f) => univ(root) ++ f
    case ObjectIntField(root, f) => univ(root) ++ f
    case ObjectField(root, f) => univ(root) ++ f
    case _: Constant[_] => Scope()
  }

  @annotation.tailrec 
  private def closure(cur: Scope)(implicit env: Environment): Scope = {
    val fields = for (o <- cur.objects; f <- cur.fields) yield f(o)

    val variables = {for (v <- cur.vars; if env.has(v)) yield v match {
      case v: ObjectVar[_] => Set(env(v))
      case v: ObjectSetVar => env(v)
      case _ => Set[Atom]()
    }}.flatten

    val more = cur ++ 
      (null: Atom) ++ 
      variables ++ 
      fields.map(univ(_)).foldLeft(Scope())(_ ++ _)

    if (more.size > cur.size)
      closure(more)
    else 
      cur
  }

  /**
   * SMT-LIB 2 translation.
   */

  private def prelude(implicit env: Environment, sc: Scope): List[String] = {
    val Scope(objects, fields, vars) = sc;

    // declare all objects
    "(declare-datatypes ((Object " + objects.map("(" + sc.encode(_) + ")").mkString(" ") + ")))" :: 
    // declare all fields
    {for (f <- fields) yield "(declare-fun " + f + {f match {
      case _: ObjectFieldDesc => " (Object) Object)"
      case _: IntFieldDesc => " (Object) Int)"
    }}}.toList :::
    // declare all variables
    {for (v <- vars; if ! env.has(v))
      yield "(declare-fun " + v + {v match {
        case _: IntVar =>  " () Int) "
        case _: BoolVar => " () Bool)"
        case _: ObjectVar[_] => " () Object)"
        case _: ObjectSetVar => " (Object) Bool)"
    }}}.toList :::
    // declare types
    "(declare-datatypes ((Type " + objects.map("(" + sc.atomId(_) + ")").mkString(" ") + ")))" ::
    "(declare-fun $type (Object) Type)" ::
    {for (o <- objects) yield "(assert (= ($type " + sc.encode(o) + ") " + sc.atomId(o) + "))"}.toList:::
    {for (v <- vars.collect{case v: ObjectVar[_] if ! env.has(v) => v}) 
      yield "(assert (or " + 
      {for (klas <- objects.map(a => if (a == null) null else a.getClass); 
          if (klas == null) || v.mayAssign(klas)) 
          yield "(= ($type " + v + ") " + sc.classId(klas) + ")"}.mkString(" ") +
    "))"}.toList :::
    // declare field values
    {for (o <- objects; f <- fields) 
      yield "(assert (= (" + f + " " + sc.encode(o) + ") " + {f match {
        case f: ObjectFieldDesc => atom(f(o))
        case f: IntFieldDesc => integer(f(o))
      }} + 
    "))"}.toList :::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   * 
   * Super-normal default logic is decided to saturating context in the order
   * the judgements are supplied.
   * 
   * Initial scope of objects is used to make sound equality theory for objects.
   */
  def solve(f: Formula, defaults: List[Formula] = Nil, initial: Set[Atom] = Set(), checkNext: Boolean = true)
    (implicit env: Environment = DefaultEnv) = {
    implicit val scope = closure(univ(f :: defaults) ++ initial)

    debug("\n *** SMT SOLVING STATISTICS *** ")
    debug("SCOPE: " + scope);
    debug("# DEFAULTS: " + defaults.size)
    debug("INITIAL SCOPE: " + initial.size)
    debug("# FORMULA CLAUSES: " + f.clauses.size)
    val start = System.currentTimeMillis;

    val solver = new Z3 //with Logging
  
    for (s <- prelude) solver.command(s)  
    for (clause <- f.clauses) 
      solver.assert(formula(clause))
  
    try {
      // invariant: the model is consistent 
      if (! solver.check) throw UnsatException
      for (d <- defaults) {
        solver.push;
        solver.assert(formula(d));
        if (! solver.check) solver.pop;
      }
      assert (solver.check)
      val result = model(solver.model)
     
      debug(" *** SUCCESS SOLVING TIME: " + (System.currentTimeMillis - start) + " ms")
      
      if (checkNext && solver.next) 
        warning("Multiple assignments") 

      result
    } catch {
      case e =>
        debug(" *** FAILED SOLVING TIME: " + (System.currentTimeMillis - start) + " ms")
        throw e;
    } finally { 
      solver.close;
    }
  }   
  

  private def model(model: String)(implicit env: Environment, sc: Scope) = {
    // parse model
    var result = env;
    val PREFIX = "((\"model\" \"";
    val SUFFIX = "\"))";
    val defines = model.substring(PREFIX.size, model.size - SUFFIX.size);
    val defs = defines.split("\\(define ");
    for (d <- defs; if d.size > 0 && ! d.startsWith("(")) {
      val List(name, value) = d.split("\\)|\\s").toList;
      for (v <- sc.vars; if name == v.toString)
        v match {
          case v: IntVar => result = result + (v -> BigInt(value))
          case v: BoolVar => result = result + (v -> value.toBoolean)
          case v: ObjectVar[_] => result = result + (v -> sc.decode(value))
          case v: ObjectSetVar => 
            // TODO: better S-expression parsing
            throw new RuntimeException("not implemented")
        }
    }
    result
  }
}

