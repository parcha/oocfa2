package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._

import dx.dex.file.EncodedField
import scala.collection._

//FIXME: Hack relying on FieldSpec = SFieldSpec = IFieldSpec
sealed abstract case class FieldSlot(val spec:FieldSpec) {
  final val typ : Instantiable = Type(spec.getType).asInstanceOf[Instantiable]
}
object FieldSlot {
  final class Known(val encoded: EncodedField) extends FieldSlot(encoded.getRef) {
    lazy val props = prop.Range(prop.Domain.Field, encoded.getAccessFlags)
  }
  final class Unknown(spec:FieldSpec) extends FieldSlot(spec)
  
  implicit def wrap(encoded: EncodedField) = new Known(encoded)
}



/**
 * See @RefType for the explanation of why this is mutable
 */
final class FieldSlotSet(val parent:FieldSlotSet)
extends mutable.HashSet[FieldSlot] with LinkSet[FieldSlot, FieldSlotSet] with mutable.SynchronizedSet[FieldSlot] {
  def apply(spec:FieldSpec) = get(spec).get
  def get(spec:FieldSpec) : Option[FieldSlot] = {
    for(f <- this)
      if(f.spec == spec) return Some(f)
    return None
  }
  def contains(spec:FieldSpec) = this exists {_.spec == spec}
  override def iterator = super[LinkSet].iterator
  
  def getOrRegister(spec: FieldSpec) : FieldSlot = get(spec) match {
    case Some(s) => s
    case None    =>
      val s = new FieldSlot.Unknown(spec)
      this += s
      s
  }
}

final class FieldMap
(final val self : FieldMap.M = new immutable.HashMap[FieldSlot, Val_])
extends immutable.MapProxy[FieldSlot, Val_] with FieldMap.ProxyFactoried {
  def contains(spec:FieldSpec) = this exists {_._1.spec == spec }
}
object FieldMap
extends MapProxyFactory[FieldSlot, Val_, immutable.Map[FieldSlot, Val_], FieldMap] (immutable.Map(), new FieldMap(_))