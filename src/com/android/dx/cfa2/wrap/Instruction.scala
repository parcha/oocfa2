package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.code.{Insn => RawInsn, _}
import dx.cfa2
import cfa2._
import `val`._
import adt.Cacher

import scala.collection._

sealed case class Instruction(val raw:RawInsn) extends Immutable with NotNull {
  def operation:ROperator = ROperator(raw.getOpcode)
  def opcode = operation.opcode
  def sources = raw.getSources
  def sourceTs = operation.sourceTypes
  def result = raw.getResult
  def resultT = Type(operation.resultType)
  def catchTs = raw.getCatches
  def branches = (catchTs.size > 0) || opcode.isInstanceOf[ROpCodes.Branches]
  def position = raw.getPosition
  override lazy val toString = raw.toHuman
}
object Instruction extends Cacher[RawInsn, Instruction] {
  private def intern(raw:RawInsn) = {
    val i = raw match {
      case raw:CstInsn => new Constant(raw)
      case _           => new Instruction(raw)
    }
    cache.cache(raw, i)
  }
  implicit def wrap(raw:RawInsn) = cache cachedOrElse (raw, intern(raw))
  implicit def unwrap(instr:Instruction) = instr.raw
  
  sealed class Constant(override val raw: CstInsn) extends Instruction(raw) {
    require(constant != null)
    def constant = raw.getConstant
  }
}