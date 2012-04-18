package cap.scalasmt

/**
 * Angelic choice statement for arithmetic.
 * @author kuat
 */
object ChoiceStmt {
  import SMT._

  def choose(spec: IntVar => Formula) = {
    val x = Var.makeInt
    val out = solve(spec(x))
    out(x).toInt;
  }

  def choose(spec: (IntVar, IntVar) => Formula) = {
    val x = Var.makeInt
    val y = Var.makeInt
    val out = solve(spec(x, y))
    (out(x).toInt, out(y).toInt)
  }

  def choose(spec: (IntVar, IntVar, IntVar) => Formula) = {
    val x = Var.makeInt
    val y = Var.makeInt
    val z = Var.makeInt
    val out = solve(spec(x,y,z))
    (out(x).toInt, out(y).toInt, out(z).toInt)
  }
}
