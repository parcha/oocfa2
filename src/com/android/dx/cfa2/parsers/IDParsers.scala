package com.android.dx.cfa2.parsers

import com.android.dx.cfa2
import cfa2.`val`.Type
import scala.util.parsing
import parsing.combinator._
import parsing.input._

class IDParsers extends RichParsers {
  
  val id = ("(?x)"+ // enhance readability
            "(?: \\p{Alpha}      |"+ //start with letter or...
            "    [\\$_] \\p{Alpha}"+ //...symbol+letter
            ")"+
            "[\\w\\$]*" //end with as many id-chars as we like
            ).r ^^ ID
  
  val qual_id = repsep(id, ".") ^^ {_ reduceLeft {_ compose _}}
  
  val meth_id = qual_id ||| (((qual_id <~ ".") ~! ("<clinit>" | "<init>")) ^^ {
    case id ~ name => ID(id.raw + "." + name)
  })
  
  lazy val type_id: Parser[TypeIDExpr] = {
    import TypeIDExpr._
    val prim = ("int" | "byte" | "short" | "long" |
                "float" | "double" |
                "char" | "boolean") ^^ Prim
    val klass = qual_id ^^ Klass
    // FIXME: Syntax should really be type_id <~ "[]", but can't get Packrat to behave
    val array = ("[" ~> type_id) ^^ Array
    positioned (array | prim | klass)
  }
  
  val method_sig = "method_sig" !!! {
    val paramTs = repsep (type_id, ",") ^^ {_ map {Type(_)}}
    val empty_sig = meth_id ^^ {MethodSig(_, List())}
    empty_sig ||| meth_id ~! ("(" ~> paramTs <~ ")") ^^ {
      case m ~ paramTs => MethodSig(m, paramTs)
    }
  }
  
  /*val hex: Parser[String] = """[0-9a-f]"""r
  val descriptor = {
    val base = """[BCDFIJSZV]"""r
    val obj = ("(?:L (?<classname>"+
                      id+
                      "(?:/"+id+")*"+ //Can't end on a /
                   ")"+
                 ";"+
               ")")r
    val pseudo = """(?:addr|null)"""r
    // Putting everything together...
    ("["*) | psuedo
  }*/
}
object IDParsers extends IDParsers