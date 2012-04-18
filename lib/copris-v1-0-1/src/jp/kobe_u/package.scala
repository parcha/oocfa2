package jp.kobe_u

/**
 * Provides classes for Constraint Programming in Scala.
 *
 * ==OVERVIEW==
 * The following is a sample program in Copris DSL (Domain Specific Language).
 * {{{
 * object FirstStep {
 *   import jp.kobe_u.copris._
 *   import jp.kobe_u.copris.dsl._
 * 
 *   def main(args: Array[String]) = {
 *     val x = int('x, 0, 7)
 *     val y = int('y, 0, 7)
 *     add(x + y === 7)
 *     add(x * 2 + y * 4 === 20)
 *     if (find) {
 *       println(solution)
 *     }
 *   }
 * }
 * }}}
 */
package object copris {
}
