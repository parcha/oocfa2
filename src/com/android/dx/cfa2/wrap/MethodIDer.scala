package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.`type`.Prototype

sealed abstract class MethodIDer extends Immutable with NotNull {
  def identifies(m: Method) : Boolean
}

final case class NameMethodID(name: String) extends MethodIDer {
  def identifies(m: Method) = m.name == name
}

final case class DescriptiveMethodID(name: String, proto: Prototype) extends MethodIDer {
  def identifies(m: Method) = m.name == name &&
                              m.getEffectiveDescriptor().equals(proto)
}

final case object AccessibleMethodIDer extends MethodIDer {
  import dx.cfa2.prop.Properties
  @inline
  private def isAccessible(m: Method) = {
    /*def hasFinalClass = classForMethod(m) match {
      case Some(c) => Class(c) is Properties.Final
      case None    => false // Conservative assumption
    }*/
    (m is Properties.Public) ||
    ((m is Properties.Protected)/* && !hasFinalClass*/)
  }
  def identifies(m: Method) = isAccessible(m)
}