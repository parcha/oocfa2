/*
 * This program is a part of Copris software package.
 * http://bach.istc.kobe-u.ac.jp/copris/
 */

import jp.kobe_u.copris._

/**
 * First-step program in Copris
 */
object FirstStep {
  import jp.kobe_u.copris.dsl._
  def main(args: Array[String]) = {
    val x = int('x, 0, 7)
    val y = int('y, 0, 7)
    add(x + y === 7)
    add(x * 2 + y * 4 === 20)
    if (find) {
      println(solution)
    }
  }
}

/**
 * 4-Queens problem solver
 * @see [[http://en.wikipedia.org/wiki/Eight_queens_puzzle]]
 */
object Queens4 {
  import jp.kobe_u.copris.dsl._
  def main(args: Array[String]) = {
    int('q(1), 1, 4)
    int('q(2), 1, 4)
    int('q(3), 1, 4)
    int('q(4), 1, 4)
    add(Alldifferent('q(1), 'q(2), 'q(3), 'q(4)))
    add(Alldifferent('q(1)+1, 'q(2)+2, 'q(3)+3, 'q(4)+4))
    add(Alldifferent('q(1)-1, 'q(2)-2, 'q(3)-3, 'q(4)-4))
    if (find) {
      println(solution)
    }
  }
}

/**
 * n-Queens problem solver
 * @see [[http://en.wikipedia.org/wiki/Eight_queens_puzzle]]
 */
object Queens {
  import jp.kobe_u.copris.dsl._
  def queens(n: Int) = {
    for (i <- 1 to n) int('q(i), 1, n)
    add(Alldifferent((1 to n).map(i => 'q(i))))
    add(Alldifferent((1 to n).map(i => 'q(i) + i)))
    add(Alldifferent((1 to n).map(i => 'q(i) - i)))
    if (find) {
      do {
        println(solution)
      } while (findNext)
    }
  }
  def main(args: Array[String]) = {
    val n = if (args.size > 0) args(0).toInt else 4
    queens(n)
  }
}

/**
 * n-Queens problem solver
 * comparing Sat4j and MiniSat
 * @see [[http://en.wikipedia.org/wiki/Eight_queens_puzzle]]
 */
object QueensBench {
  import jp.kobe_u.copris.sugar._
  import jp.kobe_u.copris.sugar.dsl._
  def queens(n: Int) = {
    for (i <- 1 to n) int('q(i), 1, n)
    add(Alldifferent((1 to n).map(i => 'q(i))))
    add(Alldifferent((1 to n).map(i => 'q(i) + i)))
    add(Alldifferent((1 to n).map(i => 'q(i) - i)))

    var time = scala.compat.Platform.currentTime
    if (find) {
      println(solution)
    }
    time = scala.compat.Platform.currentTime - time
    println(time)

    use(MiniSat)
    time = scala.compat.Platform.currentTime
    if (find) {
      println(solution)
    }
    time = scala.compat.Platform.currentTime - time
    println(time)
  }
  def main(args: Array[String]) = {
    val n = if (args.size > 0) args(0).toInt else 4
    queens(n)
  }
}

/**
 * 3 x 3 Magic square problem solver
 * @see [[http://en.wikipedia.org/wiki/Magic_square]]
 */
object MagicSquare3 {
  import jp.kobe_u.copris.dsl._
  def main(args: Array[String]) = {
    int('x11, 1, 9); int('x12, 1, 9); int('x13, 1, 9)
    int('x21, 1, 9); int('x22, 1, 9); int('x23, 1, 9)
    int('x31, 1, 9); int('x32, 1, 9); int('x33, 1, 9)
    add(Alldifferent('x11,'x12,'x13,'x21,'x22,'x23,'x31,'x32,'x33))
    add('x11 + 'x12 + 'x13 === 15)
    add('x21 + 'x22 + 'x23 === 15)
    add('x31 + 'x32 + 'x33 === 15)
    add('x11 + 'x21 + 'x31 === 15)
    add('x12 + 'x22 + 'x32 === 15)
    add('x13 + 'x23 + 'x33 === 15)
    add('x11 + 'x22 + 'x33 === 15)
    add('x13 + 'x22 + 'x31 === 15)
    if (find) {
      do {
        println(solution('x11, 'x12, 'x13).mkString(""))
        println(solution('x21, 'x22, 'x23).mkString(""))
        println(solution('x31, 'x32, 'x33).mkString(""))
        println
      } while (findNext)
    }
  }
}

/**
 * n x n Magic square problem solver
 * @see [[http://en.wikipedia.org/wiki/Magic_square]]
 */
object MagicSquare {
  import jp.kobe_u.copris.dsl._
  def magicSquare(n: Int) = {
    val n2 = n * n
    val sum = n * (n2+1) / 2
    val xs = for (i <- 1 to n; j <- 1 to n) yield 'x(i,j)
    xs.foreach(int(_, 1, n2))
    add(Alldifferent(xs))
    for (i <- 1 to n) {
      val xs = for (j <- 1 to n) yield 'x(i,j)
      add(Add(xs) === sum)
    }
    for (j <- 1 to n) {
      val xs = for (i <- 1 to n) yield 'x(i,j)
      add(Add(xs) === sum)
    }
    add(Add(for (i <- 1 to n) yield 'x(i,i)) === sum)
    add(Add(for (i <- 1 to n) yield 'x(i,n-i+1)) === sum)
    if (find) {
      for (i <- 1 to n) {
        val as = for (j <- 1 to n) yield solution('x(i,j))
        println(as.map("%3d".format(_)).mkString(""))
      }
    }
  }
  def main(args: Array[String]) = {
    val n = if (args.size > 0) args(0).toInt else 3
    magicSquare(n)
  }
}

/**
 * Knight's tour problem solver
 * MiniSat is used in this program.
 * @see [[http://en.wikipedia.org/wiki/Knight_tour]]
 */
object KnightTour {
  import jp.kobe_u.copris.sugar.dsl._
  def knightTour(n: Int) = {
    val xs = for (i <- 1 to n; j <- 1 to n) yield 'x(i, j)
    int(xs, 1, n*n)
    add(Alldifferent(xs))
    val moves = Seq((-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1))
    for (i <- 1 to n; j <- 1 to n) {
      val cs = for {
        (di, dj) <- moves
        val i1 = i + di; val j1 = j + dj
        if 1 <= i1 && i1 <= n && 1 <= j1 && j1 <= n
      } yield 'x(i,j) + 1 === 'x(i1,j1) ||
              ('x(i,j) === n*n && 'x(i1,j1) === 1)
      add(Or(cs))
    }
    use(sugar.MiniSat)
    if (find) {
      for (i <- 1 to n) {
        val as = for (j <- 1 to n) yield solution('x(i,j))
        println(as.map("%3d".format(_)).mkString(""))
      }
    }
  }
  def main(args: Array[String]) = {
    val n = if (args.size > 0) args(0).toInt else 6
    knightTour(n)
  }
}

/**
 * Perfect square packing problem solver.
 * This program finds a tiling of given 21 squares with different sizes
 * fitting in one large square whose size is 112 x 112.
 * MiniSat is used in this program.
 * @see [[http://en.wikipedia.org/wiki/Squaring_the_square]]
 */
object PerfectSquare {
  import jp.kobe_u.copris.sugar.dsl._
  def main(args: Array[String]) = {
    val size = 112
    val s =
      List(2,4,6,7,8,9,11,15,16,17,18,19,24,25,27,29,33,35,37,42,50)
    val n = s.size
    for (i <- 0 to n-1) {
      int('x(i), 0, size - s(i))
      int('y(i), 0, size - s(i))
    }
    for (i <- 0 to n-1; j <- i+1 to n-1) {
      add('x(i) + s(i) <= 'x(j) || 'x(j) + s(j) <= 'x(i) ||
          'y(i) + s(i) <= 'y(j) || 'y(j) + s(j) <= 'y(i))
    }
    use(sugar.MiniSat)
    if (find) {
      val a = Array.ofDim[Char](size, size)
      for {i <- 0 to n-1
           x <- solution('x(i)) to solution('x(i))+s(i)-1
           y <- solution('y(i)) to solution('y(i))+s(i)-1}
        a(x)(y) = ('A' to 'Z')(i)
      for (x <- 0 to size-1)
        println((0 to size-1).map(a(x)(_)).mkString(""))
    }
  }
}

/**
 * Square packing problem solver.
 * This program finds a square of the minimum size containing
 * all n squares whose sizes are from 1 x 1 to n x n.
 * MiniSat is used in this program.
 */
object SquarePacking {
  import jp.kobe_u.copris.sugar.dsl._
  def showSolution(n: Int, size: Int) {
    val a = Array.ofDim[Char](size, size)
    for (x <- 0 to size-1; y <- 0 to size-1)
      a(x)(y) = '.'
    for {
      i <- 1 to n
      x <- solution('x(i)) to solution('x(i))+i-1
      y <- solution('y(i)) to solution('y(i))+i-1
    } a(x)(y) = ('A' to 'Z')((i-1) % 26)
    for (x <- 0 to size-1)
      println((0 to size-1).map(a(x)(_)).mkString(""))
  }
  def squares(n: Int) = {
    val area = (1 to n).map(i => i*i).sum
    var size = math.ceil(math.sqrt(area)).toInt
    var found = false
    while (! found) {
      println(size)
      init
      for (i <- 1 to n) {
        int('x(i), 0, size - i)
        int('y(i), 0, size - i)
      }
      for (i <- 1 to n; j <- i+1 to n) {
        add('x(i) + i <= 'x(j) || 'x(j) + j <= 'x(i) ||
            'y(i) + i <= 'y(j) || 'y(j) + j <= 'y(i))
      }
      add('x(n-1) <= 'x(n) && 'y(n-1) <= 'y(n))
      use(sugar.MiniSat)
      if (find) {
        showSolution(n, size)
        found = true
      } else {
        size = size + 1
      }
    }
  }
  def main(args: Array[String]) = {
    val n = if (args.size > 0) args(0).toInt else 4
    squares(n)
  }
}

/**
 * Open-Shop Scheduling problem solver.
 * MiniSat is used in this program.
 */
object OSS {
  import jp.kobe_u.copris.sugar.dsl._
  val gp03_01 = Seq(
    Seq(661,   6, 333),
    Seq(168, 489, 343),
    Seq(171, 505, 324))
  val gp10_10 = Seq(
    Seq(185,   2,  86,   6, 320,  21, 234, 132,  13,   1),
    Seq(282,   1,   3,  55,   1,   2,   2,  55, 586,  13),
    Seq( 20,   3,  13, 490,   2, 458,   2,   6,   5,   1),
    Seq(  1,   3,  45,   9,   1, 100,   1, 161,   3, 676),
    Seq(  6,   1,   1, 178, 667,  92,   1,   1,  52,   1),
    Seq(283,   2, 573,  19,   4,  41,   1,   1,   1,  75),
    Seq( 14, 596,  88,   1,   1,   1,   2, 195, 101,   1),
    Seq(  3,  12,   5,   1,   1, 101, 419, 137,  91, 230),
    Seq(  5, 324,   1,   1,   2, 183, 332,   4, 147,   1),
    Seq(201,  56, 185, 240,   1,   1,   6, 308,   1,   1))
  var pt: Seq[Seq[Int]] = null
  def n = pt.size
  def lb = pt.map(_.sum).max
  def ub =
    (0 until n).map(k => (0 until n).map(i => pt(i)((i + k) % n)).max).sum
  def main(args: Array[String]) = {
    pt = gp03_01
    // pt = gp10_10
    int('makespan, lb, ub)
    minimize('makespan)
    for (i <- 0 until n; j <- 0 until n) {
      int('s(i,j), 0, ub)
      add('s(i,j) + pt(i)(j) <= 'makespan)
    }
    for (i <- 0 until n) {
      for (j <- 0 until n; l <- j+1 until n)
        add('s(i,j) + pt(i)(j) <= 's(i,l) ||
            's(i,l) + pt(i)(l) <= 's(i,j))
    }
    for (j <- 0 until n) {
      for (i <- 0 until n; k <- i+1 until n)
        add('s(i,j) + pt(i)(j) <= 's(k,j) ||
            's(k,j) + pt(k)(j) <= 's(i,j))
    }
    use(sugar.MiniSat)
    if (findOpt) {
      println(solution)
    }
  }
}
