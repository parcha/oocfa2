package com.android.dx.cfa2.wrap

import com.android.dx
import dx.dex.file.{ClassDefItem => RawClass}

import dx.cfa2
import cfa2._
import prop.Properties._

final case class Class(val raw: RawClass) extends Immutable with NotNull {
  lazy val props = prop.Range(prop.Domain.Class, raw.getAccessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  override def toString = raw.getThisClass.toHuman+"@"+raw.getSourceFile.getString
}
object Class {
  implicit def wrap(raw:RawClass) = new Class(raw)
  implicit def unwrap(c:Class) = c.raw
}