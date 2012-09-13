package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.code.{BasicBlock => RawBB}
import dx.cfa2
import cfa2._
import scala.collection._

final case class BasicBlock(val raw:RawBB, implicit val parent: Method) extends Immutable with NotNull {
  def label = raw.getLabel
  lazy val canCatch = raw.hasExceptionHandlers
  
  // FIXME: Conservatively assume all may loop, in case this logic is wrong
  lazy val mayLoop : Boolean = true/*{
    val traversed: mutable.Set[BasicBlock] = mutable.Set()
    def traverseSuccs(bb: BasicBlock) : Boolean = {
      traversed += bb
      if (bb.successors contains this) return true
      for(s <- bb.successors if !(traversed contains s))
        if(traverseSuccs(s)) return true
      return false
    }
    traverseSuccs(this)
  }*/
  
  lazy val predecessors = {
    val preds = parent.rop.labelToPredecessors(label)
    new BasicBlockSet(
      (for(i <- 0 until preds.size) yield parent.blocks(preds.get(i))).toSet)
  }
  lazy val successors = {
    val succs = raw.getSuccessors
    new BasicBlockSet(
      (for(i <- 0 until succs.size) yield parent.blocks(succs.get(i))).toSet)
  }
  
  lazy val alt_succ = raw.getSecondarySuccessor match {
    case -1 => None
    case n  => Some(successors(n))
  }
  lazy val prim_succ = raw.getPrimarySuccessor match {
    case -1 => None
    case n  => Some(successors(n))
  }
  
  def first_ins: Instruction = raw.getFirstInsn
  def last_ins: Instruction = raw.getLastInsn
  
  lazy val handlers: immutable.Set[BasicBlock] =
    (successors filter {_.first_ins.opcode == ROpCodes.MOVE_EXCEPTION}).toSet
  
  import `val`._
  def handlersFor(t:Type): immutable.Set[BasicBlock] =
    handlers filter {(succ: BasicBlock) => {
      val first = succ.first_ins
      // Pick out the successors which start with a handler for the type t
      // TODO: Should it be exact, or should it be a subtype?
      first.resultT == t
    }}
  
  lazy val dump = raw.dump
  override def toString = raw.toString
  override def equals(that: Any) = raw.equals(that)
  override val hashCode = raw.hashCode
}
object BasicBlock {
  //TODO: does this need to be threadsafe?
  private val cache = {
    type C = Cached[RawBB, BasicBlock, MutableConcurrentMap]
    new C#Map with C#Cacher
  }
  private def intern(raw:RawBB, parent: Method) = cache.cache(raw, new BasicBlock(raw, parent))
  
  def wrap(raw: RawBB, parent: Method) = cache cachedOrElse (raw, intern(raw, parent))
  implicit def unwrap(bb:BasicBlock) = bb.raw
}

final class BasicBlockSet private[wrap] (bbs: Set[BasicBlock])
extends SetProxy[BasicBlock] with Immutable with NotNull {
  // Put the blocks in their labeled order
  private val map = {
    val build = new mutable.HashMap[Int, BasicBlock]
    for(bb <- bbs)
      build(bb.label) = bb
    immutable.HashMap(build.toSeq:_*)
  }
  val self = bbs
  def apply(label: Int) = map(label)
}