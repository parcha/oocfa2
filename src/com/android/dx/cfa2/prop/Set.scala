package com.android.dx.cfa2.prop

import com.android.dx
import dx.cfa2._
import prop.Properties._

import scala.collection.{Set => CSet, _}

class Set private[prop] (props: Iterable[Property]) extends immutable.BitSet.BitSet1(Set.mkSet(props)) {
  private[prop] def this(props: Property*) = this(props)
  final def contains(props: Property*) : Boolean =
    props forall (prop =>
      this contains prop.bit)
}
object Set {
  
  implicit def toSet(props: Property*) = new Set(props)
  private implicit def mkSet(props: Iterable[Property]) = {
    var builder = 0
    for(prop <- props)
      builder |= prop.bitfield
    builder
  }
}