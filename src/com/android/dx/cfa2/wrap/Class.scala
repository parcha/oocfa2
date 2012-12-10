package com.android.dx.cfa2.wrap

import com.android.dx
import dx.dex.file.{ClassDefItem => RawClass}
import dx.rop.cst.CstType
import dx.cfa2
import cfa2._
import prop._
import Properties._
import `val`._

sealed trait ClassDesc extends Immutable with NotNull {
  def typ: Instantiable
  final def is(prop: Property) = {
    assert(Domain.Class contains prop)
    _is(prop)
  }
  def _is(prop: Property): Tri
  final val isAccessible: Tri = {
    val locallyAccessible =
    	(this is Public) |
    	((this is Protected) & !(this is Final))
    typ.klass match {
      case null  => locallyAccessible
      case klass => klass.getEnclosingClass() match {
        case null   => locallyAccessible
        case parent => ReflClass(parent).isAccessible & locallyAccessible
      } 
    }
  }
}
object ClassDesc {
  implicit val strawmanComparator = new java.util.Comparator[ClassDesc] {
    def compare(c1, c2) = c1.typ.descriptor.compareTo(c2.typ.descriptor)
  }
  implicit def strawmanOrdering[T <: ClassDesc] = mkCovariantOrdering[ClassDesc, T](strawmanComparator)
}

sealed trait DalvikClassDesc extends ClassDesc {
  def spec: CstType
  final lazy val typ: Instantiable = Type(spec.getClassType).asInstanceOf[Instantiable]
}

final case class Class(raw: RawClass) extends DalvikClassDesc {
  lazy val spec = raw.getThisClass
  lazy val props = prop.Range(prop.Domain.Class, raw.getAccessFlags)
  def _is(prop) = props contains prop
  override def toString = spec.toHuman+"@"+raw.getSourceFile.getString
}
object Class {
  implicit def wrap(raw:RawClass) = new Class(raw)
  implicit def unwrap(c:Class) = c.raw
}

final case class GhostClass(spec: CstType) extends DalvikClassDesc {
  private[this] lazy val props = typ.klass match {
    case null  => null
    case klass => Domain.Class.fromJModifiers(klass.getModifiers())
  }
  def _is(prop) =
    if(props == null) Tri.U
    else props(prop)
}
object GhostClass {
  implicit def wrap(spec:CstType) = {
    // Upgrade to ReflClass if we can
    val ghost = new GhostClass(spec)
    if(ghost.typ.klass != null)
      ReflClass(ghost.typ.klass)
    else
      ghost
  }
  implicit def unwrap(c:Class) = c.spec
}

import java.lang.{Class => JClass}
final /*implicit*/ case class ReflClass(refl: JClass[_]) extends ClassDesc {
  lazy val typ = Type(refl).asInstanceOf[Instantiable]
  lazy val props = Domain.Class.fromJModifiers(refl.getModifiers())
  def _is(prop) = props(prop)
}