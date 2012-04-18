package com.android.dx.cfa2.wrap

import com.android.dx
import dx.cf.iface.{Method => Raw}
import dx.rop.code.RopMethod

import dx.cfa2
import cfa2._
import prop.Properties._

import scala.collection._

final case class Method(val raw:Raw, val rop:RopMethod) extends Immutable with NotNull {
  lazy val blocks: BasicBlockSet = {
    val bbs = rop.getBlocks
    new BasicBlockSet(
      for(i <- 0 until bbs.size) yield BasicBlock.wrap(bbs.get(i), this))
  }
  def firstBlock = blocks(rop.getFirstLabel)
  
  lazy val props = prop.Range(prop.Domain.Method, raw.getAccessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  
  def name = raw.getName.getString
  def arity = raw.getEffectiveDescriptor.getParameterTypes.size
  def accessFlags = raw.getAccessFlags
  def attributes(attr: String) = raw.getAttributes.findFirst(attr)
  lazy val dump = rop.dump
  override lazy val toString = name+"/"+arity+"@"+raw.getDefiningClass.toHuman
}
object Method {
  def wrap(raw:Raw, rop:RopMethod) = new Method(raw, rop)
  implicit def unwrap(m:Method) = m.raw
}