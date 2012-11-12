package com.android.dx.cfa2.wrap

import com.android.dx
import dx.dex.file.{ClassDefItem => RawClass}
import dx.rop.cst.CstType
import dx.cfa2
import cfa2._
import prop.Properties._
import `val`.Type

sealed trait ClassDesc extends Immutable with NotNull {
  def spec: CstType
  final def typ: Type = spec.getClassType
}

final case class Class(raw: RawClass) extends ClassDesc {
  def spec = raw.getThisClass
  lazy val props = prop.Range(prop.Domain.Class, raw.getAccessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  override def toString = spec.toHuman+"@"+raw.getSourceFile.getString
}
object Class {
  implicit def wrap(raw:RawClass) = new Class(raw)
  implicit def unwrap(c:Class) = c.raw
}

final case class GhostClass(spec: CstType) extends ClassDesc
object GhostClass {
  implicit def wrap(spec:CstType) = new GhostClass(spec)
  implicit def unwrap(c:Class) = c.spec
}