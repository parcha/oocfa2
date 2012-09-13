package com.android.dx.cfa2.wrap

import com.android.dx
import dx.cf.iface.{Method => Raw}
import dx.rop.code.RopMethod

import dx.cfa2
import cfa2._
import `val`._
import prop.Properties._

import scala.collection._

sealed trait MethodDesc extends Immutable with NotNull {
  final def name = nat.getName.getString
  def prototype: dx.rop.`type`.Prototype
  final lazy val arity = argTs.size
  final lazy val argTs =
    for(i <- 0 until prototype.getParameterTypes.size)
      // TODO: Should this be ParameterFrameTypes?
      yield Type(prototype.getParameterTypes.get(i)).asInstanceOf[Instantiable]
  
  def parent: dx.rop.cst.CstType
  def nat: dx.rop.cst.CstNat
  final lazy val isInstanceInit = nat.isInstanceInit
  final lazy val isClassInit = nat.isClassInit
}

final case class Method(val raw:Raw, val rop:RopMethod) extends MethodDesc {
  lazy val blocks: BasicBlockSet = {
    val bbs = rop.getBlocks
    new BasicBlockSet(
      (for(i <- 0 until bbs.size) yield BasicBlock.wrap(bbs.get(i), this)).toSet)
  }
  def firstBlock = blocks(rop.getFirstLabel)
  
  lazy val props = prop.Range(prop.Domain.Method, accessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  def parent = raw.getDefiningClass
  def accessFlags = raw.getAccessFlags
  def prototype = raw.getEffectiveDescriptor
  def attributes(attr: String) = raw.getAttributes.findFirst(attr)
  
  def nat = raw.getNat
  lazy val dump = rop.dump
  override lazy val toString = name+"/"+arity+"@"+parent.toHuman
}
object Method {
  def wrap(raw:Raw, rop:RopMethod) = new Method(raw, rop)
  implicit def unwrap(m:Method) = m.raw
}

final case class GhostMethod(val spec:MethodSpec) extends MethodDesc {
  // FIXME: we need a way to "getEffectiveDescriptor" here...
  def prototype = spec.getPrototype
  def parent = spec.getDefiningClass
  def nat = spec.getNat
}
object GhostMethod {
  implicit def wrap(spec:MethodSpec) = new GhostMethod(spec)
  implicit def unwrap(gm:GhostMethod) = gm.spec
}