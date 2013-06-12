package com.android.dx

import com.android.dx
import cfa2._
import scala.collection.{parallel=>par, _}
import com.android.dx.cfa2.analysis.CFA2Analysis

package object cfa2 {

  object SysProps extends scala.sys.SystemProperties
  
  /* ============= Convenience ============ */
  import dx.rop.cst._
  def liftConstant(cst:TypedConstant) = {
    import `val`._
    cst match {
    case c:CstInteger   => Val.Atom(INT.instance(c.getValue))
    case c:CstLong      => Val.Atom(LONG.instance(c.getValue))
    case c:CstFloat     => Val.Atom(FLOAT.instance(c.getValue))
    case c:CstDouble    => Val.Atom(DOUBLE.instance(c.getValue))
    case c:CstString    => Val.Atom(STRING.instance(c.getString))
    case c:CstKnownNull => Val.Atom(NULL.singleton)
    case c:CstType      =>
      val v = Type(c.getClassType).asInstanceOf[Instantiable].klass match {
        case null  => CLASS.unknown(Val.Bottom)
		case klass => CLASS.instance(klass)
      }
      Val.Atom(v)
  }
  }
  
  /* ============ Exceptions =================== */
  /** This is an assertion/requirement stemming from reasonable program input/configuration **/
  final class PresumptionException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
  
  def presume(presumption: Boolean,
              msg: =>String = "presumption failed") = {
    if(!presumption)
      throw new PresumptionException(msg)
  }
  
  import scala.reflect.{ClassTag, classTag}
  def presumably[E <: Exception : ClassTag](act: Unit=>Unit) = presumably[E, Unit](act)
  def presumably[E <: Exception : ClassTag, R](act: Unit=>R): R = {
    try {
      act()
    } catch {
      case e:E =>
        if(classTag[E] >:> ClassTag(e.getClass))
          throw new PresumptionException("presumption failed", e)
        else throw e
    }
  }
  
  final case class InternalError(msg: String = null, cause: Throwable = null) extends RuntimeException(msg, cause)
  
  /* ============= Interfacing with Java codebase ======== */
  /*import dx.rop.code.{BasicBlock=>BB}
  implicit def toBBVisitor(f: (BB, BB)=>Unit) : BB.Visitor = {
    new BB.Visitor() {
      def visitBlock(v:BB, parent:BB) = f(v,parent)
    }
  }
  implicit def toBBVisitor(f: (BB)=>Unit) : BB.Visitor = {
    new BB.Visitor() {
      def visitBlock(v:BB, parent:BB) = f(v)
    }
  }
  def toBBParentVisitor(f: (BB)=>Unit) : BB.Visitor = {
    new BB.Visitor() {
      def visitBlock(v:BB, parent:BB) = f(parent)
    }
  }
  
  import dx.ssa.{SsaInsn=>Insn, _}
  def toMoveInsnVisitor(f: NormalSsaInsn=>Unit) : Insn.Visitor = {
    new Insn.Visitor() {
      def visitMoveInsn(i:NormalSsaInsn) = f(i)
      def visitPhiInsn(i:PhiInsn) = {}
      def visitNonMoveInsn(i:NormalSsaInsn) = {}
    }
  }
  def toNonMoveInsnVisitor(f: NormalSsaInsn=>Unit) : Insn.Visitor = {
    new Insn.Visitor() {
      def visitMoveInsn(i:NormalSsaInsn) = {}
      def visitPhiInsn(i:PhiInsn) = {}
      def visitNonMoveInsn(i:NormalSsaInsn) = f(i)
    }
  }
  implicit def toPhiInsnVisitor(f: PhiInsn=>Unit) : Insn.Visitor = {
    new Insn.Visitor() {
      def visitMoveInsn(i:NormalSsaInsn) = {}
      def visitPhiInsn(i:PhiInsn) = f(i)
      def visitNonMoveInsn(i:NormalSsaInsn) = {}
    }
  }*/
  
  type FieldSpec = dx.rop.cst.CstFieldRef
  type IFieldSpec = FieldSpec
  type SFieldSpec = FieldSpec
  
  type MethodSpec = dx.rop.cst.CstBaseMethodRef
  type IMethodSpec = MethodSpec
  type SMethodSpec = MethodSpec
  
  type ClassSpec = dx.rop.cst.CstType
  
  /* ============= Class-loading/lifting hackery =========== */
  import java.io.File
  val analysisClasspath : immutable.Seq[File] = immutable.Seq(
    "."
  ) map (new File(_))
  assert(analysisClasspath forall (_.exists))
  object BuiltinAnalysisClassLoader extends AnalysisClassLoader(ClassLoader.getSystemClassLoader,
                                                                (analysisClasspath map (_.toURL)))
  val analysisMirror = scala.reflect.runtime.universe.runtimeMirror(BuiltinAnalysisClassLoader)
  
  /* ============= String repr stuff ========= */
  
  type Row[E] = GenIterable[E]
  type Col[E] = GenSeq[E]
  type Matrix[E] = Row[Col[E]]
  
  def linearize(row:Row[String], twixt:String="") : String = {
    val str = new StringBuilder
    for(col <- row) {
      str append col
      str append twixt
    }
    str.delete(str.length-twixt.length, str.length)
    return str result
  }
  
  def linearizeMat(matrix:Matrix[String], twixtCols:String="", twixtRows:String="") : String = {
    val columnized =
    for(row <- matrix) yield
      linearize(row, twixtRows)
    return linearize(columnized, twixtCols)
  }
  
  def normalize(matrix : Matrix[String], spacing:Int=0) : Matrix[String] = {
    val rows = matrix.size
    require(rows > 0)
    val cols = matrix.head.length
    
    val maxes =
    for(c <- 0 until cols) yield {
      val lens = for(row <- matrix) yield row(c).length
      lens.max
    }
    
    for(row <- matrix)
      yield for(c <- 0 until cols) yield {
        if(c < cols-1)
        	String.format("%-"+(maxes(c)+spacing)+"s", row(c))
        else row(c)
      }
  }
  
  
  /* ============= Logging =============== */
  trait Dumpable {
    import java.io.PrintStream
    def dump(ps: PrintStream) = ps println this.toString
  }
  
}
