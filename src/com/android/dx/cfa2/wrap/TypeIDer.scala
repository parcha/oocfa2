package com.android.dx.cfa2.wrap

import com.android.dx
import dx.cfa2
import cfa2._
import cfa2.`val`._
import parsers._

sealed abstract class TypeIDer extends Immutable with NotNull {
  def identifies(t:Type): Tri
  lazy val reasoning: String = this toString
}
object TypeIDer {
  object Sugar {
    final case class SugarString(s: String) extends NotNull {
      def unary_! = Sugar.parse(s)
    }
    implicit def wrap(s: String) = SugarString(s)
    implicit def parse(s: String) = ByName(IDParsers.parse(IDParsers.id, s).get)
  }
  sealed trait Unique extends TypeIDer
  
  final case class ByName(id: ID) extends Unique {
    def identifies(t:Type) = t.name == id.raw
  }
  final case class SubOf(sup: Type) extends TypeIDer {
    def identifies(t:Type) = t < sup
  }
  final case class SuperOf(sub: Type) extends TypeIDer {
    def identifies(t:Type) = t > sub
  }
}