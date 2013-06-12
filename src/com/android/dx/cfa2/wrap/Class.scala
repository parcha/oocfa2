package com.android.dx.cfa2.wrap

import com.android.dx
import dx.dex.file.{ClassDefItem => RawClass}
import dx.rop.cst.CstType
import dx.cfa2
import cfa2._
import prop._
import Properties._
import `val`._
import adt.Cacher

sealed abstract class _ClassDesc[+Self <: _ClassDesc[Self]] extends Immutable with NotNull { _:Self=>
  def typ: Instantiable
  final def is(prop: Property) = {
    assert(Domain.Class contains prop)
    _is(prop)
  }
  protected[this] def _is(prop: Property): Tri
  
  type Reflected = this.type with _ReflClass[Self]
  
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
object _ClassDesc extends StrawmanOrdering[ClassDesc] {
  def compare(c1, c2) = c1.typ.descriptor.compareTo(c2.typ.descriptor)
}

sealed trait _DalvikClassDesc[+Self <: _DalvikClassDesc[Self]] extends _ClassDesc[Self] { _:Self =>
  val spec: ClassSpec
  lazy val typ: Instantiable = Type(spec.getClassType).asInstanceOf[Instantiable]
}

sealed case class DalvikClass private (raw: RawClass) extends _DalvikClassDesc[DalvikClass] {
  val spec = raw.getThisClass
  private[this] lazy val props = prop.Range(prop.Domain.Class, raw.getAccessFlags)
  protected[this] def _is(prop) = props contains prop
  //def annotations = raw.getClassAnnotations()
  override def toString = spec.toHuman+"@"+raw.getSourceFile.getString
}
object DalvikClass extends Cacher[RawClass, DalvikClass] {
  private def intern(c:DalvikClass) = cache.cache(c.raw, c)
  implicit def wrap(raw:RawClass) = cache cachedOrElse (raw, {
    // Upgrade to ReflClass if we can
    val _typ = Type(raw.getThisClass.getClassType).asInstanceOf[Instantiable]
    val c =
      if(_typ.klass != null)
        new DalvikClass(raw) with _ReflClass[DalvikClass#Reflected] {
          val typ = _typ
          val refl = _typ.klass
        }
      else new DalvikClass(raw)
    intern(c)
  })
  implicit def unwrap(c:DalvikClass) = c.raw
}

sealed case class GhostClass private (spec: ClassSpec) extends _DalvikClassDesc[GhostClass] {
  private[this] lazy val props = typ.klass match {
    case null  => null
    case klass => Domain.Class.fromJModifiers(klass.getModifiers())
  }
  protected[this] def _is(prop) =
    if(props == null) Tri.U
    else props(prop)
}
object GhostClass extends Cacher[ClassSpec, GhostClass] {
  private def intern(c:GhostClass) = cache.cache(c.spec, c) 
  implicit def wrap(spec:ClassSpec) = cache cachedOrElse (spec, {
    // Upgrade to ReflClass if we can
    val _typ = Type(spec.getClassType).asInstanceOf[Instantiable]
    val c =
      if(_typ.klass != null)
        new GhostClass(spec) with _ReflClass[GhostClass#Reflected] {
          val typ = _typ
          val refl = _typ.klass
        }
      else new GhostClass(spec)
    intern(c)
  })
  implicit def unwrap(c:GhostClass) = c.spec
}


import java.lang.{Class => JClass}
sealed trait _ReflClass[+Self <: _ReflClass[Self]] extends _ClassDesc[Self] { _:Self =>
  val refl: JClass[_]
  lazy val typ = Type(refl).asInstanceOf[Instantiable]
  final override type Reflected = this.type
  private[this] lazy val props = Domain.Class.fromJModifiers(refl.getModifiers())
  final protected[this] def _is(prop) = props(prop)
}
final case class ReflClass(refl: JClass[_]) extends _ReflClass[ReflClass]
object ReflClass {
  
}