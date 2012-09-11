package com.android.dx.cfa2

import scala.util.parsing
import scala.collection._
import parsing.combinator._
import parsing.input._

package object parsers {

  trait RichParsers extends RegexParsers with PackratParsers {
    protected final val eol = "\r\n" | "\n"
    
    protected final case class ICString(s: String) {
      def i: Parser[String] = ("""(?i)\Q"""+s+"""\E""").r
    }
    protected final implicit def toICString(s:String) = ICString(s)
    
    /** At least twice */
    protected final def rep2sep[T](p: =>Parser[T], q: =>Parser[Any]): Parser[List[T]] =
      (p <~ q) ~ rep1sep(p, q) ^^ {case x~y => x::y}
    
    /*protected final class DebugParser[+T](name:String, parser:Parser[T]) extends Parser[T] {
      def apply(in:Input): ParseResult[T] = {
        val first =
          if(in.first.isWhitespace || in.first.isControl) in.first.byteValue
          else in.first
        val pos = in.pos
        val offset = in.offset
        val v = parser(in)
        System.err.println("PARSE-DEBUG: "+name+": "+
                           "first="+first+"  "+
                           "pos="+pos+"  "+
                           "offset="+offset+"  "+
                           "v="+v)
        v
      }
    }*/
    protected final implicit def toDebugWrap(name:String) = new {
      @inline
      def !!![T](p:Parser[T]) = p //log(p)(name)
    }
    
    protected final def ignorable[T](p: =>Parser[T], handle: String=>Unit=null): Parser[Option[T]] = Parser { in =>
      p(in) match {
        case Success(r,next)   => Success(Some(r),next)
        case Failure(msg,next) =>
          if(handle != null)
            handle("Ignoring failure:\n"+msg)
          Success(None, next)
        case e @ Error(_,_)    => e
      }
    }
    
    protected final def ignorable_[T](p: =>Parser[List[T]], handle: String=>Unit=null): Parser[List[T]] =
      ignorable(p, handle) ^^ {_.flatten.toList}
    
    import java.io._
    def PackratReader(f: File) =
      new PackratReader(new PagedSeqReader(immutable.PagedSeq.fromReader(new FileReader(f))))
    /*object Sugar {
      def parse[T](parser: Parser[T], s:String) =
        parser(new CharSequenceReader(s))
      def parse_[T](parser: Parser[T], s:String) = parse(parser, s) match {
        case Success(v, _) => v 
      }
    }*/
  }
  
  implicit def unwrap(id:ID) = id.raw
}