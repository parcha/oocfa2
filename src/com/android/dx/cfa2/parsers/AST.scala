package com.android.dx.cfa2.parsers

import com.android.dx.cfa2
import cfa2.`val`.Type

final case class ID private[parsers] (raw: String) extends NotNull {
  //private[parsers] def this(parts: List[ID]) = this(parts map {_.id} mkString ".")
  def compose(that: ID) = ID(this.raw+"."+that.raw)
  lazy val unqualified = raw.split('.').last
}
sealed abstract class TypeIDExpr extends scala.util.parsing.input.Positional {
  def descriptor: String
}
object TypeIDExpr {
  sealed case class Prim private[parsers] (name: String) extends TypeIDExpr {
    val descriptor = name match {
      case "boolean" => "Z"
      case "char"    => "C"
      case "byte"    => "B"
      case "short"   => "S"
      case "int"     => "I"
      case "long"    => "J"
      case "float"   => "F"
      case "double"  => "D"
    }
    override lazy val toString = name
  }
  final case class Klass private[parsers] (id:ID) extends TypeIDExpr {
    val descriptor = "L"+id.raw.replace('.','/')+";"
    override lazy val toString = id.raw
  }
  final case class Array private[parsers] (expr:TypeIDExpr) extends TypeIDExpr {
    def descriptor = "[" + expr.descriptor
    override def toString = expr.toString + "[]"
  }
}
final case class MethodSig private[parsers] (id:ID, argTs:Seq[Type]) extends NotNull {
  //def this(m:ID, argTs:Type*) = this(m, argTs)
  //def this(m:ID, argTs:ID*) = this(m, argTs map {Type(_)})
}