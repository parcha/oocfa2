package jp.kobe_u.copris

import scala.collection._

/**
 * Abstract class of expressions.
 * [[jp.kobe_u.copris.Term]]s and [[jp.kobe_u.copris.Constraint]]s are expressions.
 */
abstract class Expr

/**
 * Abstract class of terms.
 *
 * Operators defined in this class create a new expression.
 * For example, `x + y` returns a new term `Add(x, y)`
 * when `x` and `y` are terms.
 */
sealed abstract class Term extends Expr {
  /** Returns [[jp.kobe_u.copris.Neg]] of Term */
  def unary_- = Neg(this)
  /** Returns [[jp.kobe_u.copris.Add]] of Terms */
  def + (x: Term) = Add(this, x)
  /** Returns [[jp.kobe_u.copris.Add]] of Term with Int */
  def + (a: Int) = Add(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Sub]] of Terms */
  def - (x: Term) = Sub(this, x)
  /** Returns [[jp.kobe_u.copris.Sub]] of Term with Int */
  def - (a: Int) = Sub(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Mul]] of Terms */
  def * (x: Term) = Mul(this, x)
  /** Returns [[jp.kobe_u.copris.Mul]] of Term with Int */
  def * (a: Int) = Mul(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Div]] of Terms */
  def / (x: Term) = Div(this, x)
  /** Returns [[jp.kobe_u.copris.Div]] of Term by Int */
  def / (a: Int) = Div(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Mod]] of Terms */
  def % (x: Term) = Mod(this, x)
  /** Returns [[jp.kobe_u.copris.Mod]] of Term by Int */
  def % (a: Int) = Mod(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Max]] of Terms */
  def max (x: Term) = Max(this, x)
  /** Returns [[jp.kobe_u.copris.Max]] of Term and Int */
  def max (a: Int) = Max(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Min]] of Terms */
  def min (x: Term) = Min(this, x)
  /** Returns [[jp.kobe_u.copris.Min]] of Term and Int */
  def min (a: Int) = Min(this, Num(a))

  /** Returns [[jp.kobe_u.copris.Eq]] of Terms */
  def === (x: Term) = Eq(this, x)
  /** Returns [[jp.kobe_u.copris.Eq]] of Term and Int */
  def === (a: Int) = Eq(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Ne]] of Terms */
  def !== (x: Term) = Ne(this, x)
  /** Returns [[jp.kobe_u.copris.Ne]] of Term and Int */
  def !== (a: Int) = Ne(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Le]] of Terms */
  def <= (x: Term) = Le(this, x)
  /** Returns [[jp.kobe_u.copris.Le]] of Term and Int */
  def <= (a: Int) = Le(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Lt]] of Terms */
  def < (x: Term) = Lt(this, x)
  /** Returns [[jp.kobe_u.copris.Lt]] of Term and Int */
  def < (a: Int) = Lt(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Ge]] of Terms */
  def >= (x: Term) = Ge(this, x)
  /** Returns [[jp.kobe_u.copris.Ge]] of Term and Int */
  def >= (a: Int) = Ge(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Gt]] of Terms */
  def > (x: Term) = Gt(this, x)
  /** Returns [[jp.kobe_u.copris.Gt]] of Term and Int */
  def > (a: Int) = Gt(this, Num(a))

  /** Returns the value of the term */
  def value(solution: Solution): Int
}
/**
 * Object of `NIL` term.
 */
object NIL extends Term {
  def value(solution: Solution): Int =
    throw new IllegalArgumentException("value of NIL is not defined")
  override def toString = "nil"
}
/**
 * Case class of number terms.
 * @param value the value of the number term
 */
case class Num(value: Int) extends Term {
  def value(solution: Solution): Int = value
  override def toString = value.toString
}
/**
 * Object of number term 0.
 */
object ZERO extends Num(0)
/**
 * Object of number term 1.
 */
object ONE extends Num(1)
/**
 * Case class of integer variables.
 * @param name the primary name of the variable
 * @param is the indices of the variable (optional)
 */
case class Var(name: String, is: String*) extends Term with Ordering[Var] {
  /** Returns a new variable with extra indices given by `is1` */
  def apply(is1: Any*) =
    Var(name, is ++ is1.map(_.toString): _*)
  /** Compares variables */
  def compare(x1: Var, x2: Var) = {
    if (x1.name != x2.name)
      x1.name.compare(x2.name)
    else if (x1.is.size != x2.is.size)
      x1.is.size.compare(x2.is.size)
    else if (x1.is == x2.is)
      0
    else
      (0 until x1.is.size).map(i => x1.is(i).compare(x2.is(i))).
        find(_ != 0).getOrElse(0)
  }
  def value(solution: Solution): Int = solution(this)
  override def toString =
    if (is.size == 0) name else is.mkString(name + "(", ",", ")")
}
object Var {
  private var count = 0
  /** Returns a new anonymous variable */
  def apply() = { count += 1; new Var("_" + count) }
}
/**
 * Case class for absolute value of term.
 */
case class Abs(x0: Term) extends Term {
  def value(solution: Solution): Int = math.abs(x0.value(solution))
}
/**
 * Case class for negation of term.
 */
case class Neg(x0: Term) extends Term {
  def value(solution: Solution): Int = - x0.value(solution)
}
/**
 * Case class for addition of terms.
 * Companion object provies other factory methods.
 */
case class Add(xs: Term*) extends Term {
  def value(solution: Solution): Int = xs.map(_.value(solution)).sum
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for addition of terms.
 */
object Add {
  def apply(xs: Iterable[Term]) = new Add(xs.toSeq: _*)
}
/**
 * Case class for subtraction of terms.
 * Companion object provies other factory methods.
 */
case class Sub(xs: Term*) extends Term {
  def value(solution: Solution): Int = xs.map(_.value(solution)) match {
    case Seq() => 0
    case Seq(a) => a
    case Seq(a, as @ _*) => a - as.sum
  }
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for subtraction of terms.
 */
object Sub {
  def apply(xs: Iterable[Term]) = new Sub(xs.toSeq: _*)
}
/**
 * Case class for multiplication of terms.
 * Companion object provies other factory methods.
 */
case class Mul(xs: Term*) extends Term {
  def value(solution: Solution): Int = xs.map(_.value(solution)).product
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
/**
 * Factory for multiplication of terms.
 */
}
object Mul {
  def apply(xs: Iterable[Term]) = new Mul(xs.toSeq: _*)
}
/**
 * Case class for division of terms.
 */
case class Div(x0: Term, x1: Term) extends Term {
  // TODO wrong for negative values
  def value(solution: Solution): Int = {
    val a0 = x0.value(solution)
    val a1 = x1.value(solution)
    a0 / a1
  }
}
/**
 * Case class for remainder of terms.
 */
case class Mod(x0: Term, x1: Term) extends Term {
  def value(solution: Solution): Int = {
    val r = x0.value(solution) % x1.value(solution)
    if (r > 0) r else r + x1.value(solution)
  }
}
/**
 * Case class for maximum of terms.
 * Companion object provies other factory methods.
 */
case class Max(xs: Term*) extends Term {
  def value(solution: Solution): Int = xs.map(_.value(solution)).max
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for maximum of terms.
 */
object Max {
  def apply(xs: Iterable[Term]) = new Max(xs.toSeq: _*)
}
/**
 * Case class for minimum of terms.
 * Companion object provies other factory methods.
 */
case class Min(xs: Term*) extends Term {
  def value(solution: Solution): Int = xs.map(_.value(solution)).min
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for minimum of terms.
 */
object Min {
  def apply(xs: Iterable[Term]) = new Min(xs.toSeq: _*)
}
/**
 * Case class for if expressions.
 */
case class If(c: Constraint, x0: Term, x1: Term) extends Term {
  def value(solution: Solution): Int =
    if (c.value(solution)) x0.value(solution) else x1.value(solution)
}

/**
 * Abstract class of constraints.
 *
 * Operators defined in this class create a new expression.
 * For example, `c && d` returns a new term `And(c, d)`
 * when `c` and `d` are constraints.
 */
sealed abstract class Constraint extends Expr {
  /** Returns [[jp.kobe_u.copris.Not]] of Constraint */
  def unary_! = Not(this)
  /** Returns [[jp.kobe_u.copris.And]] of Constraints */
  def && (c: Constraint) = And(this, c)
  /** Returns [[jp.kobe_u.copris.Or]] of Constraints */
  def || (c: Constraint) = Or(this, c)
  /** Returns [[jp.kobe_u.copris.Imp]] of Constraints */
  def ==> (c: Constraint) = Imp(this, c)
  /** Returns [[jp.kobe_u.copris.Xor]] of Constraints */
  def ^ (c: Constraint) = Xor(this, c)
  /** Returns [[jp.kobe_u.copris.Iff]] of Constraints */
  def <==> (c: Constraint) = Iff(this, c)

  /** Returns the value of the constraint */
  def value(solution: Solution): Boolean
}
/**
 * Abstract class of global constraints.
 */
sealed abstract class GlobalConstraint extends Constraint

/**
 * Object of `FALSE` constraint.
 */
object FALSE extends Constraint {
  def value(solution: Solution): Boolean = false
  override def toString = "false"
}
/**
 * Object of `TRUE` constraint.
 */
object TRUE extends Constraint {
  def value(solution: Solution): Boolean = true
  override def toString = "true"
}
/**
 * Case class of Boolean variables.
 * @param name the primary name of the variable
 * @param is the indices of the variable (optional)
 */
case class Bool(name: String, is: String*) extends Constraint with Ordering[Bool] {
  /** Returns a new variable with extra indices given by `is1` */
  def apply(is1: Any*) =
    Bool(name, is ++ is1.map(_.toString): _*)
  /** Compares variables */
  def compare(x1: Bool, x2: Bool) = {
    if (x1.name != x2.name)
      x1.name.compare(x2.name)
    else if (x1.is.size != x2.is.size)
      x1.is.size.compare(x2.is.size)
    else if (x1.is == x2.is)
      0
    else
      (0 until x1.is.size).map(i => x1.is(i).compare(x2.is(i))).
        find(_ != 0).getOrElse(0)
  }
  def value(solution: Solution): Boolean = solution(this)
  override def toString =
    if (is.size == 0) name else is.mkString(name + "(", ",", ")")
}
object Bool {
  private var count = 0
  /** Returns a new anonymous variable */
  def apply() = { count += 1; new Bool("_" + count) }
}
/**
 * Case class for logical negation of constaint.
 */
case class Not(c0: Constraint) extends Constraint {
  def value(solution: Solution): Boolean = ! c0.value(solution)
}
/**
 * Case class for conjuction of constaints.
 * Companion object provies other factory methods.
 */
case class And(cs: Constraint*) extends Constraint {
  def value(solution: Solution): Boolean = cs.forall(_.value(solution))
  override def toString = cs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for conjunction of terms.
 */
object And {
  def apply(xs: Iterable[Constraint]) = new And(xs.toSeq: _*)
}
/**
 * Case class for disjuction of constaints.
 * Companion object provies other factory methods.
 */
case class Or(cs: Constraint*) extends Constraint {
  def value(solution: Solution): Boolean = cs.exists(_.value(solution))
  override def toString = cs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for disjunction of terms.
 */
object Or {
  def apply(xs: Iterable[Constraint]) = new Or(xs.toSeq: _*)
}
/**
 * Case class for implication of constaint.
 */
case class Imp(c0: Constraint, c1: Constraint) extends Constraint {
  def value(solution: Solution): Boolean = ! c0.value(solution) || c1.value(solution)
}
/**
 * Case class for exclusive-or of constaint.
 */
case class Xor(c0: Constraint, c1: Constraint) extends Constraint {
  def value(solution: Solution): Boolean = c0.value(solution) ^ c1.value(solution)
}
/**
 * Case class for if-and-only-if of constaint.
 */
case class Iff(c0: Constraint, c1: Constraint) extends Constraint {
  def value(solution: Solution): Boolean = c0.value(solution) == c1.value(solution)
}
/**
 * Case class for equals constraints.
 */
case class Eq(x0: Term, x1: Term) extends Constraint {
  def value(solution: Solution): Boolean = x0.value(solution) == x1.value(solution)
}
/**
 * Case class for not-equals constraints.
 */
case class Ne(x0: Term, x1: Term) extends Constraint {
  def value(solution: Solution): Boolean = x0.value(solution) != x1.value(solution)
}
/**
 * Case class for less-than-or-equals constraints.
 */
case class Le(x0: Term, x1: Term) extends Constraint {
  def value(solution: Solution): Boolean = x0.value(solution) <= x1.value(solution)
}
/**
 * Case class for less-than constraints.
 */
case class Lt(x0: Term, x1: Term) extends Constraint {
  def value(solution: Solution): Boolean = x0.value(solution) < x1.value(solution)
}
/**
 * Case class for greater-than-or-equals constraints.
 */
case class Ge(x0: Term, x1: Term) extends Constraint {
  def value(solution: Solution): Boolean = x0.value(solution) >= x1.value(solution)
}
/**
 * Case class for greater-than constraints.
 */
case class Gt(x0: Term, x1: Term) extends Constraint {
  def value(solution: Solution): Boolean = x0.value(solution) > x1.value(solution)
}
/**
 * Case class for Alldifferent global constraints.
 * Companion object provies other factory methods.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Calldifferent.html]]
 */
case class Alldifferent(xs: Term*) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    val as = xs.map(_.value(solution))
    (0 until as.size).forall(i => (i+1 until as.size).forall(j => as(i) != as(j)))
  }
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for Alldifferent global constraints.
 */
object Alldifferent {
  def apply(xs: Iterable[Term]) = new Alldifferent(xs.toSeq: _*)
}
/**
 * Case class for Weightedsum global constraints.
 */
case class Weightedsum(axs: Seq[(Int,Term)], cmp: String, b: Term) extends GlobalConstraint {
  assert(cmp.matches("eq|ne|lt|le|gt|ge"))
  def value(solution: Solution): Boolean = {
    val sum = axs.map(ax => ax._1 * ax._2.value(solution)).sum
    cmp match {
      case "eq" => sum == b.value(solution)
      case "ne" => sum != b.value(solution)
      case "lt" => sum <  b.value(solution)
      case "le" => sum <= b.value(solution)
      case "gt" => sum >  b.value(solution)
      case "ge" => sum >= b.value(solution)
    }
  }
  override def toString =
    productPrefix + "(" + axs + "," + cmp + "," + b + ")"
}
/**
 * Case class for Cumulative global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Ccumulative.html]]
 */
case class Cumulative(tasks: Seq[(Term,Term,Term,Term)], limit: Term) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    def value(x: Term) = x.value(solution)
    def taskBlock(task: (Term,Term,Term,Term)) = {
      val (origin, duration, end, height) = task
      if (origin == NIL)
	(value(end) - value(duration), value(end), value(height))
      else if (end == NIL)
	(value(origin), value(origin) + value(duration), value(height))
      else
	(value(origin), value(end), value(height))
    }
    val taskBlocks = tasks.map(taskBlock(_))
    val timeBlocks = (taskBlocks.map(_._1).toSet | taskBlocks.map(_._2).toSet).toSeq.sorted.sliding(2)
    val lim = value(limit)
    timeBlocks.forall {case Seq(t0,t1) => {
      val hs = for ((o,e,h) <- taskBlocks)
	       yield if (t0 <= o && e <= t1) h else 0
      hs.sum <= lim
    }}
  }
  override def toString =
    productPrefix + "(" + tasks + "," + limit + ")"
}
/**
 * Case class for Element global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Celement.html]]
 */
case class Element(i: Term, xs: Seq[Term], xi: Term) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    val as = xs.map(_.value(solution))
    as(i.value(solution) - 1) == xi.value(solution)
  }
  override def toString =
    productPrefix + "(" + i + "," + xs + "," + xi + ")"
}
/**
 * Case class for Disjunctive global constraints.
 * Companion object provies other factory methods.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Cdisjunctive.html]]
 */
case class Disjunctive(tasks: (Term,Term)*) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    val ts = tasks.map(task => (task._1.value(solution), task._2.value(solution)))
    (0 until ts.size).forall { i =>
      (i+1 until ts.size).forall { j =>
	ts(i)._1 + ts(i)._2 <= ts(j)._1 || ts(j)._1 + ts(j)._2 <= ts(i)._1
      }
    }
  }
  override def toString =
    productPrefix + "(" + tasks + ")"
}
/**
 * Factory for Disjunctive global constraints.
 */
object Disjunctive {
  def apply(tasks: Iterable[(Term,Term)]) = new Disjunctive(tasks.toSeq: _*)
}
/**
 * Case class for LexLess global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Clex_less.html]]
 */
case class LexLess(xs: Seq[Term], ys: Seq[Term]) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    def less(as: Seq[Int], bs: Seq[Int]): Boolean =
      if (as.isEmpty || bs.isEmpty)
	as.isEmpty && ! bs.isEmpty
      else
	as.head < bs.head || (as.head == bs.head && less(as.tail, bs.tail))
    less(xs.map(_.value(solution)), ys.map(_.value(solution)))
  }
  override def toString =
    productPrefix + "(" + xs + "," + ys + ")"
}
/**
 * Case class for LexLesseq global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Clex_lesseq.html]]
 */
case class LexLesseq(xs: Seq[Term], ys: Seq[Term]) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    def lessEq(as: Seq[Int], bs: Seq[Int]): Boolean =
      if (as.isEmpty || bs.isEmpty)
	as.isEmpty
      else
	as.head < bs.head || (as.head == bs.head && lessEq(as.tail, bs.tail))
    lessEq(xs.map(_.value(solution)), ys.map(_.value(solution)))
  }
  override def toString =
    productPrefix + "(" + xs + "," + ys + ")"
}
/**
 * Case class for Nvalue global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Cnvalue.html]]
 */
case class Nvalue(count: Term, xs: Seq[Term]) extends GlobalConstraint {
  def value(solution: Solution): Boolean =
    xs.map(_.value(solution)).toSet.size == count.value(solution)
  override def toString =
    productPrefix + "(" + count + "," + xs + ")"
}
/**
 * Case class for GlobalCardinality global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Cglobal_cardinality.html]]
 */
case class GlobalCardinality(xs: Seq[Term], card: Seq[(Int,Term)]) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    val as = xs.map(_.value(solution))
    card.forall {
      case (b, c) => as.count(_ == b) == c.value(solution)
    }
  }
  override def toString =
    productPrefix + "(" + xs + "," + card + ")"
}
/**
 * Case class for GlobalCardinalityWithCosts global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Cglobal_cardinality_with_costs.html]]
 */
case class GlobalCardinalityWithCosts(xs: Seq[Term], card: Seq[(Int,Term)],
                                      table: Seq[(Int,Int,Int)], cost: Term) extends GlobalConstraint {
  def value(solution: Solution): Boolean = {
    def getCost(i: Int, a: Int) = {
      val j = card.indexWhere(_._1 == a) + 1
      table.find(t => t._1 == i && t._2 == j).get._3
    }
    val as = xs.map(_.value(solution))
    card.forall {
      case (b, c) => as.count(_ == b) == c.value(solution)
    } && {
      val cs = for (i <- 1 to as.size) yield getCost(i, as(i - 1))
      cs.sum == cost.value(solution)
    }
  }
  override def toString =
    productPrefix + "(" + xs + "," + card  + "," + table  + "," + cost + ")"
}
/**
 * Case class for Count global constraints.
 * @see [[http://www.emn.fr/z-info/sdemasse/gccat/Ccount.html]]
 */
case class Count(value: Term, xs: Seq[Term], cmp: String, count: Term) extends GlobalConstraint {
  assert(cmp.matches("eq|ne|lt|le|gt|ge"))
  def value(solution: Solution): Boolean = {
    val a = value.value(solution)
    val c = xs.map(_.value(solution)).count(_ == a)
    cmp match {
      case "eq" => c == count.value(solution)
      case "ne" => c != count.value(solution)
      case "lt" => c <  count.value(solution)
      case "le" => c <= count.value(solution)
      case "gt" => c >  count.value(solution)
      case "ge" => c >= count.value(solution)
    }
  }
  override def toString =
    productPrefix + "(" + value + "," + xs + "," + cmp + "," + count + ")"
}

/**
 * Abstract class of domains.
 * Companion object provies other factory methods.
 */
abstract class Domain {
  /** Returns lower bound value */
  def lb: Int
  /** Returns upper bound value */
  def ub: Int
  /** Checks the domain contains the given value */
  def contains(a: Int): Boolean
}
/**
 * Case class of interval domain
 */
case class IntervalDomain(lo: Int, hi: Int) extends Domain {
  assert(lo <= hi)
  def lb = lo
  def ub = hi
  def contains(a: Int): Boolean = lo <= a && a <= hi
  override def productPrefix = "Domain"
}
/**
 * Case class of set domain
 */
case class SetDomain(values: SortedSet[Int]) extends Domain {
  def lb = values.min
  def ub = values.max
  def contains(a: Int): Boolean = values.contains(a)
  override def productPrefix = "Domain"
}
/**
 * Factory for creating domains.
 */
object Domain {
  /** Returns [[jp.kobe_u.copris.IntervalDomain]] */
  def apply(lo: Int, hi: Int) =
    IntervalDomain(lo, hi)
  /** Returns [[jp.kobe_u.copris.IntervalDomain]] with singleton value */
  def apply(value: Int) =
    IntervalDomain(value, value)
  /** Returns [[jp.kobe_u.copris.SetDomain]] */
  def apply(values: Set[Int]) =
    SetDomain(SortedSet(values.toSeq: _*))
}

/**
 * Trait of CSP (Constraint Satisfaction Problem)
 */
trait CSPTrait {
  /** Adds an integer variable */
  def int(x: Var, d: Domain): Var
  /** Adds an integer variable */
  def int(x: Var, d: Set[Int]): Var =
    int(x, Domain(d))
  /** Adds an integer variable */
  def int(x: Var, lo: Int, hi: Int): Var =
    int(x, Domain(lo, hi))
  /** Adds an integer variable */
  def int(x: Var, value: Int): Var =
    int(x, Domain(value))
  /** Adds integer variables */
  def int(xs: Iterable[Term], d: Domain): Iterable[Term] = {
    xs.foreach(_ match {
      case x: Var => int(x, d)
      case _ =>
        throw new IllegalArgumentException("argument of int declaration should be a Var")
    })
    xs
  }
  /** Adds integer variables */
  def int(xs: Iterable[Term], d: Set[Int]): Iterable[Term] =
    int(xs, Domain(d))
  /** Adds integer variables */
  def int(xs: Iterable[Term], lo: Int, hi: Int): Iterable[Term] =
    int(xs, Domain(lo, hi))
  /** Adds integer variables */
  def int(xs: Iterable[Term], value: Int): Iterable[Term] =
    int(xs, Domain(value))

  /** Adds a Boolean variable */
  def bool(p: Bool): Bool
  /** Adds Boolean variables */
  def bool(ps: Iterable[Bool]): Iterable[Bool] =
    { ps.foreach(bool(_)); ps }

  /** Adds a constraint */
  def add(cs: Constraint*): Unit
  /** Adds constraints */
  def add(cs: Iterable[Constraint]): Unit =
    add(cs.toSeq: _*)

  /** Specifies objective variable to be minimized */
  def minimize(x: Var): Var
  /** Specifies objective variable to be maximized */
  def maximize(x: Var): Var

  /** Checks whether the CSP is satisfied by the solution */
  def satisfiedBy(solution: Solution): Boolean
}

/**
 * Case class of CSP (Constraint Satisfaction Problem)
 * @param variables integer variables
 * @param bools Boolean variables
 * @param dom domains of integer variables
 * @param constraints constraints
 */
case class CSP(var variables: Seq[Var] = Seq.empty,
               var bools: Seq[Bool] = Seq.empty,
               var dom: Map[Var,Domain] = Map.empty,
               var constraints: Seq[Constraint] = Seq.empty)
  extends CSPTrait {
  private var variablesSize = 0
  private var boolsSize = 0
  private var constraintsSize = 0
  /** Objective variable.  `null` if not defined */
  var objective: Var = null
  private var target = 0

  /**
   * Creates a copy of the given CSP
   * @param csp0 original CSP
   */
  def this(csp0: CSP) = {
    this(csp0.variables, csp0.bools, csp0.dom, csp0.constraints)
    objective = csp0.objective
    target = csp0.target
    commit
  }
  /**
   * Resets the CSP by setting variables, bools, dom, and
   * constraints to be empty.
   */
  def init: Unit = {
    variables = Seq.empty; bools = Seq.empty
    dom = Map.empty; constraints = Seq.empty
  }
  /**
   * Adds an integer variable
   */
  def int(x: Var, d: Domain): Var = {
    if (variables.contains(x))
      throw new IllegalArgumentException("duplicate int " + x)
    variables :+= x; dom += x -> d; x
  }
  /**
   * Adds a Boolean variable
   */
  def bool(p: Bool): Bool = {
    if (bools.contains(p))
      throw new IllegalArgumentException("duplicate bool " + p)
    bools :+= p; p
  }
  /**
   * Adds constraints
   */
  def add(cs: Constraint*): Unit =
    constraints = constraints ++ cs
  /**
   * Commits the changes made for the CSP.
   */
  def commit: Unit = {
    variablesSize = variables.size
    boolsSize = bools.size
    constraintsSize = constraints.size
  }
  /**
   * Cancels the changes made for the CSP.
   */
  def cancel: Unit = {
    variables = variables.take(variablesSize)
    bools = bools.take(boolsSize)
    constraints = constraints.take(constraintsSize)
  }
  /**
   * Returns the integer variables added after the last commit.
   */
  def variablesDelta =
    variables.drop(variablesSize)
  /**
   * Returns the Boolean variables added after the last commit.
   */
  def boolsDelta =
    bools.drop(boolsSize)
  /**
   * Returns the constraints added after the last commit.
   */
  def constraintsDelta =
    constraints.drop(constraintsSize)
  /**
   * Specifies the objective variable to be minimized
   */
  def minimize(x: Var): Var = {
    objective = x
    target = -1
    x
  }
  /**
   * Specifies the objective variable to be maximized
   */
  def maximize(x: Var): Var = {
    objective = x
    target = 1
    x
  }
  /**
   * Returns true when the minimization is specified
   */
  def isMinimize = target < 0
  /**
   * Returns true when the maximization is specified
   */
  def isMaximize = target > 0
  /* */
  def satisfiedBy(solution: Solution): Boolean = {
    variables.forall {
      x => dom(x).contains(x.value(solution))
    } && constraints.forall {
      c => c.value(solution)
    }
  }
  /**
   * Returns the readable String representation of the CSP
   */
  def output: String = {
    val sb = new StringBuilder()
    for (x <- variables) dom(x) match {
      case d: IntervalDomain =>
        sb.append("int(" + x + "," + d.lo + "," + d.hi + ")\n")
      case d: SetDomain =>
        sb.append("int(" + x + "," + d + ")\n")
    }
    for (p <- bools)
      sb.append("bool(" + p + ")\n")
    for (c <- constraints)
      sb.append(c.toString + "\n")
    if (isMinimize)
      sb.append("minimize(" + objective + ")\n")
    else if (isMaximize)
      sb.append("maximize(" + objective + ")\n")
    sb.toString
  }
}

/*
 * Statement (this trait is not used)
 * Constraint class is also a Statement
 */
/*
trait Statement
case class IntStatement(x: Var, d: Domain) extends Statement
case class BoolStatement(p: Bool) extends Statement
case class ConstraintStatement(c: Constraint) extends Statement
case class MinimizeStatement(x: Var) extends Statement
case class MaximizeStatement(x: Var) extends Statement
*/
