package com.android.dx.cfa2.wrap

import com.android.dx
import dx.cfa2
import cfa2._
import `val`._
import prop._
import adt.{LinkSet, MapProxyFactory, Cacher}
import Properties._
import dx.dex.file.EncodedField

import scala.collection._

import java.lang.reflect.{Field => JField}

sealed abstract class _FieldSlotDesc[+Self <: _FieldSlotDesc[Self]]
extends Immutable with NotNull { _:Self =>
  def name: String
  def parent: ClassDesc
  val typ: Instantiable
  
  type Reflected = Self with _ReflFieldSlotDesc[Self]
  def ifReflected[R](_then: =>Reflected=>R)(_else: =>R): R = this match {
    case self:Reflected => _then(self)
    case _              => _else
  }
  val reflected = ifReflected[Option[Reflected]] {Some(_)} {None}
  
  final val isFinal = is(Final) | ((parent is Final) == Tri.T)
  final def is(prop: Property) = {
    assert(Domain.Field contains prop)
    _is(prop)
  }
  protected[this] def _is(prop: Property): Tri
}

sealed trait _ReflFieldSlotDesc[+Self <: _ReflFieldSlotDesc[Self]]
extends _FieldSlotDesc[Self] { _:Self =>
  val refl: JField
  final override type Reflected = Self
  final override def ifReflected[R](_then: =>Reflected=>R)(_else: =>R) = _then(this)
  final override val reflected = Some(this)
  
  protected[this] lazy val props = Domain.Field.fromJModifiers(refl.getModifiers)
  protected[this] def _is(prop) = props(prop)
}

//FIXME: Hack relying on FieldSpec = SFieldSpec = IFieldSpec
sealed abstract case class FieldSlot(spec:FieldSpec) extends _FieldSlotDesc[FieldSlot] {
  final val typ : Instantiable = Type(spec.getType).asInstanceOf[Instantiable]
  final def name = spec.getNat.getName.getString
  final val parent = GhostClass.wrap(spec.getDefiningClass)
}
object FieldSlot extends Cacher[FieldSpec, FieldSlot] {
  sealed class Known private[FieldSlot] (val encoded: EncodedField) extends FieldSlot(encoded.getRef) {
    protected[this] lazy val props = prop.Range(prop.Domain.Field, encoded.getAccessFlags)
    protected[this] def _is(prop) = props contains prop
  }
  object Known {
    def apply(encoded:EncodedField) = wrap(encoded)
  }
  sealed class Unknown private[FieldSlot] (spec:FieldSpec) extends FieldSlot(spec) {
    protected[this] def _is(prop) = Tri.U
  }
  object Unknown {
    def apply(spec:FieldSpec) = wrap(spec)
  }
  
  private def intern(s:FieldSlot) = cache.cache(s.spec, s)
  implicit def wrap(encoded: EncodedField) = cache cachedOrElse (encoded.getRef,
    getReflection[Known](encoded.getRef) { _refl =>
      new Known(encoded) with _ReflFieldSlotDesc[Reflected[Known]] {
        val refl = _refl
      }
    } {new Known(encoded)})
  implicit def wrap(spec: FieldSpec) = cache cachedOrElse (spec,
    getReflection[Unknown](spec) { _refl =>
      new Unknown(spec) with _ReflFieldSlotDesc[Reflected[Unknown]] {
        val refl = _refl
      }
    } {new Unknown(spec)})
  
  // HACK: no recursive type aliases in Scala :/
  private type Reflected[S <: FieldSlot] = S with _ReflFieldSlotDesc[S]
  private def getReflection[S <: FieldSlot]
                           (spec:FieldSpec)(constr: =>JField=>Reflected[S])(default: =>S): S = {
    val parent = GhostClass.wrap(spec.getDefiningClass)
    val name = spec.getNat.getName.getString
    parent.typ.klass match {
      case null  => default
      case klass => klass.getDeclaredField(name) match {
        case null  => default
        case field => constr(field)
      }
    }
  }
    
}



/**
 * See @RefType for the explanation of why this is mutable
 */
final class FieldSlotSet(val parent:FieldSlotSet)
extends mutable.HashSet[FieldSlot] with LinkSet[FieldSlot, FieldSlotSet] with mutable.SynchronizedSet[FieldSlot] {
  def apply(spec:FieldSpec) = get(spec).get
  def apply(name:String) = get(name).get
  
  def get(spec:FieldSpec) : Option[FieldSlot] = {
    for(f <- this)
      if(f.spec == spec) return Some(f)
    return None
  }
  // FIXME: This is both wrong (at least missing ".") and inefficient; thankfully it's unused atm
  def get(name:String) = {
    def fullSpecName(spec:FieldSpec) = spec.getDefiningClass().getType().toHuman + specName(spec)
    def specName(spec:FieldSpec) = spec.getNat().getName().getString()
    val filter: FieldSpec => Boolean =
      if(name contains '.')
        {fullSpecName(_) == name}
      else
        {specName(_) == name}
    val candidates = this filter ((f:FieldSlot) => filter(f.spec))
    presume(candidates.size <= 1)
    candidates.size match {
      case 0 => None
      case 1 => Some(candidates.head)
    }
  }
  
  def contains(spec:FieldSpec) = this exists {_.spec == spec}
  override def iterator = super[LinkSet].iterator
  
  def getOrRegister(spec: FieldSpec) : FieldSlot = get(spec) match {
    case Some(s) => s
    case None    =>
      val s = FieldSlot.Unknown(spec)
      this += s
      s
  }
}

final class FieldMap
(final val self : FieldMap.M = new immutable.HashMap[FieldSlot, Val_])
extends immutable.MapProxy[FieldSlot, Val_] with FieldMap.Factoried {
  def contains(spec:FieldSpec) = this exists {_._1.spec == spec }
  
}
object FieldMap
extends MapProxyFactory[FieldSlot, Val_, immutable.Map, FieldMap] (immutable.Map(), new FieldMap(_))