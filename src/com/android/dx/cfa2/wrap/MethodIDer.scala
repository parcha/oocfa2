package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.`type`.Prototype
import dx.cfa2
import cfa2._
import parsers._
import scala.collection._

sealed abstract class MethodIDer extends Immutable with NotNull {
  import MethodIDer._
  def identifies(m: MethodDesc) : Tri
  lazy val reasoning: String = this toString
  
  final def &&& (that:MethodIDer): MethodIDer = &(immutable.Seq(this, that))
  final def ||| (that:MethodIDer): MethodIDer = |(immutable.Seq(this, that))
}
object MethodIDer {
  object Sugar {
    final case class SugarString(s: String) extends NotNull {
      def unary_~ = Sugar.parse(s)
    }
    implicit def wrap(s: String) = SugarString(s)
    implicit def parse(s: String) = ByName(IDParsers.parse(IDParsers.id, s).get)
  }
  // TODO: Make this significant
  sealed trait Unique extends MethodIDer
  
  final case class ByName(id: ID) extends Unique {
    def this(s:String) = this(IDParsers.parse(IDParsers.id, s).get)
    def identifies(m: MethodDesc) = m.id == id
  }
  object ByName { def apply(s:String) = new ByName(s) }
  
  final case class LaxlyByName(id: ID) {
    def this(s:String) = this(IDParsers.parse(IDParsers.id, s).get)
    def identifies(m: MethodDesc) = m.id.unqualified == id.unqualified
  }
  object LaxlyByName { def apply(s:String) = new LaxlyByName(s) }
  
  final case class BySignature(sig:MethodSig) extends Unique {
    def this(s:String) = this(IDParsers.parse(IDParsers.method_sig, s).get)
    def identifies(m: MethodDesc) = {
      m.id == sig.id &&
      (m.argTs sameElements sig.argTs)
    }
  }
  object BySignature { def apply(s:String) = new BySignature(s) }
  
  final case class LaxlyBySignature(sig:MethodSig) extends Unique {
    def this(s:String) = this(IDParsers.parse(IDParsers.method_sig, s).get)
    def identifies(m: MethodDesc) = {
      m.id.unqualified == sig.id.unqualified &&
      (m.argTs sameElements sig.argTs)
    }
  }
  object LaxlyBySignature { def apply(s:String) = new LaxlyBySignature(s) }
  
  final case class ByPrototype(id: ID, proto: Prototype) extends Unique {
    def identifies(m: MethodDesc) = m.id == id &&
                                    m.prototype == proto
  }
  
  // Combinators
  private case class &(iders: Iterable[MethodIDer]) extends MethodIDer {
    def identifies(m) = Tri.all(iders map {_ identifies m})
    override lazy val reasoning = iders mkString " & "
  }
  private case class |(iders: Iterable[MethodIDer]) extends MethodIDer {
    def identifies(m) = Tri.any(iders map {_ identifies m})
    override lazy val reasoning = iders mkString " | "
  }
  
  // Convenience
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