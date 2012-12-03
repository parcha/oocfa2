package com.android.dx.cfa2.wrap

import com.android.dx
import dx.dex.file.{ClassDefItem => RawClass}
import dx.rop.cst.CstType
import dx.cfa2
import cfa2._
import prop.Properties._
import `val`._

sealed trait ClassDesc extends Immutable with NotNull {
  val typ: Instantiable
  val isFinal: Tri
}
object ClassDesc {
  implicit val strawmanComparator = new java.util.Comparator[ClassDesc] {
    def compare(c1, c2) = c1.typ.descriptor.compareTo(c2.typ.descriptor)
  }
  implicit def strawmanOrdering[T <: ClassDesc] = mkCovariantOrdering[ClassDesc, T](strawmanComparator)
}

sealed trait DalvikClassDesc extends ClassDesc {
  val spec: CstType
  final val typ: Instantiable = Type(spec.getClassType).asInstanceOf[Instantiable]
}

final case class Class(raw: RawClass) extends DalvikClassDesc {
  val spec = raw.getThisClass
  lazy val props = prop.Range(prop.Domain.Class, raw.getAccessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  val isFinal = Tri.lift(is(Final))
  override def toString = spec.toHuman+"@"+raw.getSourceFile.getString
}
object Class {
  implicit def wrap(raw:RawClass) = new Class(raw)
  implicit def unwrap(c:Class) = c.raw
}

final case class GhostClass(spec: CstType) extends DalvikClassDesc {
  val isFinal = typ.klass match {
    case null  => Tri.U
    case klass => Tri.lift(java.lang.reflect.Modifier.isFinal(klass.getModifiers()))
  }
}
object GhostClass {
  implicit def wrap(spec:CstType) = new GhostClass(spec)
  implicit def unwrap(c:Class) = c.spec
}

import java.lang.{Class => JClass}
final /*implicit*/ case class ReflClass(refl: JClass[_]) extends ClassDesc {
  val typ = Type(refl).asInstanceOf[Instantiable]
  val isFinal: Tri = Final.testJModifiers(refl.getModifiers())
}