package jp.kobe_u.copris

/**
 * Case class for vectors of terms.
 * Companion object provies other factory methods.
 */
case class Vec(xs: Seq[Term]) extends Iterable[Term] {
  /** Returns the iterator of elements */
  def iterator = xs.iterator
  /** Returns the i-th element */
  def apply(i: Int) = xs(i)
  /** Converts to the sequence of terms */
  override def toSeq = xs
  /** Concatenates vectors */
  def ++(v: Vec) = Vec(xs ++ v.xs)
  /** Negates the element terms (see [[jp.kobe_u.copris.Neg]]) */
  def unary_- =
    Vec(xs.map(Neg(_)))
  /** Element-wise addition of vectors (see [[jp.kobe_u.copris.Add]]) */
  def + (that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => x
      case (NIL, y) => y
      case (x, y) => Add(x, y)
    })
  }
  /** Element-wise subtraction of vectors (see [[jp.kobe_u.copris.Sub]]) */
  def - (that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => x
      case (NIL, y) => Neg(y)
      case (x, y) => Sub(x, y)
    })
  }
  /** Element-wise multiplication of vectors (see [[jp.kobe_u.copris.Mul]]) */
  def * (that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => x
      case (NIL, y) => y
      case (x, y) => Mul(x, y)
    })
  }
  /** Element-wise division of vectors (see [[jp.kobe_u.copris.Div]]) */
  def / (that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => x
      case (NIL, y) => Div(ONE, y)
      case (x, y) => Div(x, y)
    })
  }
  /** Element-wise remainder of vectors (see [[jp.kobe_u.copris.Mod]]) */
  def % (that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => ZERO
      case (NIL, y) => Mod(ONE, y)
      case (x, y) => Mod(x, y)
    })
  }
  /** Element-wise max of vectors (see [[jp.kobe_u.copris.Max]]) */
  def max(that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => x
      case (NIL, y) => y
      case (x, y) => Max(x, y)
    })
  }
  /** Element-wise min of vectors (see [[jp.kobe_u.copris.Min]]) */
  def min(that: Vec) = {
    val pairs = xs.zipAll(that.xs, NIL, NIL)
    Vec(pairs.map {
      case (x, NIL) => x
      case (NIL, y) => y
      case (x, y) => Min(x, y)
    })
  }
  /** Applies [[jp.kobe_u.copris.Add]] of the term for each element */
  def + (x: Term): Vec = Vec(xs.map(Add(_, x)))
  /** Applies [[jp.kobe_u.copris.Add]] of the integer for each element */
  def + (x: Int): Vec = this + Num(x)
  /** Applies [[jp.kobe_u.copris.Sub]] of the term for each element */
  def - (x: Term): Vec = Vec(xs.map(Sub(_, x)))
  /** Applies [[jp.kobe_u.copris.Sub]] of the integer for each element */
  def - (x: Int): Vec = this - Num(x)
  /** Applies [[jp.kobe_u.copris.Mul]] of the term for each element */
  def * (x: Term): Vec = Vec(xs.map(Mul(_, x)))
  /** Applies [[jp.kobe_u.copris.Mul]] of the integer for each element */
  def * (x: Int): Vec = this * Num(x)
  /** Applies [[jp.kobe_u.copris.Div]] of the term for each element */
  def / (x: Term): Vec = Vec(xs.map(Div(_, x)))
  /** Applies [[jp.kobe_u.copris.Div]] of the integer for each element */
  def / (x: Int): Vec = this / Num(x)
  /** Applies [[jp.kobe_u.copris.Mod]] of the term for each element */
  def % (x: Term): Vec = Vec(xs.map(Mod(_, x)))
  /** Applies [[jp.kobe_u.copris.Mod]] of the integer for each element */
  def % (x: Int): Vec = this % Num(x)
  /** Applies [[jp.kobe_u.copris.Max]] of the term for each element */
  def max(x: Term): Vec = Vec(xs.map(Max(_, x)))
  /** Applies [[jp.kobe_u.copris.Max]] of the integer for each element */
  def max(x: Int): Vec = this max Num(x)
  /** Applies [[jp.kobe_u.copris.Min]] of the term for each element */
  def min(x: Term): Vec = Vec(xs.map(Min(_, x)))
  /** Applies [[jp.kobe_u.copris.Min]] of the integer for each element */
  def min(x: Int): Vec = this min Num(x)
  /** Returns [[jp.kobe_u.copris.Add]] of all elements */
  def sum = Add(xs: _*)
  /** Returns [[jp.kobe_u.copris.Mul]] of all elements */
  def prod = Mul(xs: _*)
  /** Returns [[jp.kobe_u.copris.Min]] of all elements */
  def min = Min(xs: _*)
  /** Returns [[jp.kobe_u.copris.Max]] of all elements */
  def max = Max(xs: _*)
  /** Returns dot product of vectors */
  def dot(that: Vec) = (this * that).sum
  /** Returns dot product of vectors */
  @deprecated("use :* instead", "1.0.1")
  def *+ (that: Vec) = dot(that)
  /** Returns dot product of vectors */
  def :* (that: Vec) = dot(that)
  /** Duplicates the vector `n` times */
  def dup(n: Int): Vec = if (n <= 1) this else Vec(xs ++ dup(n-1).xs)
}
/**
 * Factory for vectors
 */
object Vec {
  /** Constructs a vector from sequence of integers */
  def apply(is: Iterable[Int]) = new Vec(is.toSeq.map(Num(_)))
  /** Constructs a vector from terms */
  def apply(x: Term, xs: Term*) = new Vec(x +: xs)
  /** Constructs a vector from integers */
  def apply(i: Int, is: Int*) = new Vec((i +: is).map(Num(_)))
  /** Constructs a vector of indexed variables */
  def apply(x: Var, is: Seq[Any]) = new Vec(is.map(x(_)))
}

/**
 * Case class for matrices of terms.
 * Companion object provies other factory methods.
 */
case class Matrix(vs: Seq[Vec]) extends Iterable[Vec] {
  /** Returns the iterator of row vectors */
  def iterator = vs.iterator
  /** Returns the i-th row vector */
  def apply(i: Int) = vs(i)
  /** Returns the (i,j) element */
  def apply(i: Int, j: Int) = vs(i)(j)
  /** Returns the i-th row vector */
  def row(i: Int) = vs(i)
  /** Returns the j-th column vector */
  def col(j: Int) = Vec(vs.map(_(j)))
  /** Returns the sub-matrix */
  def subMatrix(i: Int, j: Int, rows: Int, cols: Int) =
    Matrix((i until i+rows).map(i0 => Vec((j until j+cols).map(j0 => vs(i0)(j0)))))
  /** Transposes the matrix */
  def transpose =
    Matrix(vs.toList.map(_.xs.toList).transpose.map(Vec(_)))
  /** Returns the flattened vector */
  def toVec = Vec(vs.flatten)
  /** Negates the each element [[jp.kobe_u.copris.Neg]] */
  def unary_- = Matrix(vs.map(- _))
  /** Element-wise addition */
  def + (that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x + y }
  /** Element-wise subtraction */
  def - (that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x - y }
  /** Element-wise multiplication */
  def * (that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x * y }
  /** Element-wise division */
  def / (that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x / y }
  /** Element-wise remainder */
  def % (that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x % y }
  /** Element-wise max */
  def max(that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x max y }
  /** Element-wise min */
  def min(that: Matrix) =
    vs.zip(that.vs).map { case (x, y) => x min y }
  /** Addition of the term for each element */
  def + (x: Term): Matrix = Matrix(vs.map(_ + x))
  /** Addition of the integer for each element */
  def + (x: Int): Matrix = this + Num(x)
  /** Subtraction of the term for each element */
  def - (x: Term): Matrix = Matrix(vs.map(_ - x))
  /** Subtraction of the integer for each element */
  def - (x: Int): Matrix = this - Num(x)
  /** Multiplication of the term for each element */
  def * (x: Term): Matrix = Matrix(vs.map(_ * x))
  /** Multiplication of the integer for each element */
  def * (x: Int): Matrix = this * Num(x)
  /** Division by the term for each element */
  def / (x: Term): Matrix = Matrix(vs.map(_ / x))
  /** Division by the integer for each element */
  def / (x: Int): Matrix = this / Num(x)
  /** Remainder by the term for each element */
  def % (x: Term): Matrix = Matrix(vs.map(_ % x))
  /** Remainder by the integer for each element */
  def % (x: Int): Matrix = this % Num(x)
  /** Max of the term for each element */
  def max(x: Term): Matrix = Matrix(vs.map(_ max x))
  /** Max by the integer for each element */
  def max(x: Int): Matrix = this max Num(x)
  /** Min of the term for each element */
  def min(x: Term): Matrix = Matrix(vs.map(_ min x))
  /** Min by the integer for each element */
  def min(x: Int): Matrix = this min Num(x)
  /** Returns the sum of row vectors */
  def sum = vs.reduceLeft(_ + _)
  /** Returns the product of row vectors */
  def prod = vs.reduceLeft(_ * _)
  /** Returns the max of row vectors */
  def max = vs.reduceLeft(_ max _)
  /** Returns the min of row vectors */
  def min = vs.reduceLeft(_ min _)
  /** Returns the product with the given vector */
  def :* (v: Vec): Vec = Vec(vs.map(_ dot v))
  /** Returns the product with the given matrix */
  def :* (m: Matrix): Matrix = {
    val t = m.transpose
    Matrix(vs.map(t :* _))
  }
  /** Returns the product with the given vector */
  @deprecated("use :* instead", "1.0.1")
  def *+ (v: Vec): Vec = Vec(vs.map(_ dot v))
  /** Returns the product with the given matrix */
  @deprecated("use :* instead", "1.0.1")
  def *+ (m: Matrix): Matrix = {
    val t = m.transpose
    Matrix(vs.map(v => Vec(t.map(v dot _).toSeq)))
  }
}
/**
 * Factory for vectors
 */
object Matrix {
  /** Constructs a matrix from vectors */
  def apply(v: Vec, vs: Vec*) = new Matrix(v +: vs)
  /** Constructs a matrix of indexed variables */
  def apply(x: Var, is: Seq[Any], js: Seq[Any]) =
    new Matrix(is.map(i => Vec(js.map(j => x(i,j)))))
}
