package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.`type`.Prototype
import dx.cfa2
import cfa2._
import parsers._

sealed abstract class MethodIDer extends Immutable with NotNull {
  def identifies(m: MethodDesc) : Tri
  lazy val reasoning: String = this toString
}
object MethodIDer {
  object Sugar {
    final case class SugarString(s: String) extends NotNull {
      def unary_~ = Sugar.parse(s)
    }
    implicit def wrap(s: String) = SugarString(s)
    implicit def parse(s: String) = ByName(IDParsers.parse(IDParsers.id, s).get)
  }
  sealed trait Unique extends MethodIDer
  
  final case class ByName(id: ID) extends Unique {
    def this(s:String) = this(IDParsers.parse(IDParsers.id, s).get)
    def identifies(m: MethodDesc) = m.name == id.raw
  }
  object ByName { def apply(s:String) = new ByName(s) }
  
  final case class BySignature(sig:MethodSig) extends Unique {
    def this(s:String) = this(IDParsers.parse(IDParsers.method_sig, s).get)
    def identifies(m: MethodDesc) =
      m.name == sig.id.raw &&
      m.arity == sig.argTs.length &&
      (m.argTs sameElements sig.argTs)
  }
  object BySignature { def apply(s:String) = new BySignature(s) }
  
  final case class ByPrototype(id: ID, proto: Prototype) extends Unique {
    def identifies(m: MethodDesc) = m.name == id.raw &&
                                    m.prototype == proto
  }
  
  final case object Accessible extends MethodIDer {
    import dx.cfa2.prop.Properties
    @inline
    private def isAccessible(m: Method) = {
      /*def hasFinalClass = classForMethod(m) match {
        case Some(c) => Class(c) is Properties.Final
        case None    => false // Conservative assumption
      }*/
      (m is Properties.Public) ||
      ((m is Properties.Protected)/* && !hasFinalClass*/)
    }
    def identifies(m: MethodDesc) = m match {
      case m: Method => isAccessible(m)
      case _         => Tri.U
    }
  }
}