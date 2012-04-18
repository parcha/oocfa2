/*
 * This program is a part of Copris software package.
 * http://bach.istc.kobe-u.ac.jp/copris/
 */

import jp.kobe_u.copris._

/**
 * Sudoku puzzle solver
 * @see [[http://www.nikoli.com/en/puzzles/sudoku/rule.html]]
 * @see [[http://puzzle.gr.jp]]
 */
object Sudoku {
  import jp.kobe_u.copris.dsl._
  def solve(m: Int, n: Int, puzzle: Seq[Seq[Int]]) = {
    for (i <- 0 until n; j <- 0 until n)
      int('x(i,j), 1, n)
    for (i <- 0 until n)
      add(Alldifferent((0 until n).map('x(i,_))))
    for (j <- 0 until n)
      add(Alldifferent((0 until n).map('x(_,j))))
    for (i <- 0 until n by m; j <- 0 until n by m) {
      val xs = for (di <- 0 until m; dj <- 0 until m) yield 'x(i+di,j+dj)
      add(Alldifferent(xs))
    }
    for (i <- 0 until n; j <- 0 until n; if puzzle(i)(j) > 0)
      add('x(i,j) === puzzle(i)(j))
    if (find) {
      for (i <- 0 until n)
	println((0 until n).map(j => solution('x(i,j))).mkString(" "))
    }
  }
  def main(args: Array[String]) = {
    /* http://puzzle.gr.jp */
    val puzzle = Seq(
      Seq(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Seq(0, 4, 3, 0, 0, 0, 6, 7, 0),
      Seq(5, 0, 0, 4, 0, 2, 0, 0, 8),
      Seq(8, 0, 0, 0, 6, 0, 0, 0, 1),
      Seq(2, 0, 0, 0, 0, 0, 0, 0, 5),
      Seq(0, 5, 0, 0, 0, 0, 0, 4, 0),
      Seq(0, 0, 6, 0, 0, 0, 7, 0, 0),
      Seq(0, 0, 0, 5, 0, 1, 0, 0, 0),
      Seq(0, 0, 0, 0, 8, 0, 0, 0, 0))
    solve(3, 9, puzzle)
  }
}
