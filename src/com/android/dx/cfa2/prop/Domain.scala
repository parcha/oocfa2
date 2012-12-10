package com.android.dx.cfa2.prop

import com.android.dx
import dx.rop.code.AccessFlags
import dx.cfa2._
import prop.Properties._
import scala.collection.{Set => CSet, _}
import java.lang.reflect.{Member => JMember, Modifier => JModifier}

sealed abstract class Domain protected (val self: CSet[Property]) extends SetProxy[Property] {
  def this(props: Property*) = this(props.toSet)
  
  final def fromJMember(m: JMember): Map[Property, Tri] = immutable.Map(
    (for(p <- self) yield p match {
      case p:Reflectable => (p, Tri(p.testJMember(m)))
      case p => (p, Tri.U)
    }).toSeq:_*)
  final def fromJModifiers(mods: Int): Map[Property, Tri] = immutable.Map(
    (for(p <- self) yield p match {
      case p:JModifierReflectable => (p, Tri(p.testJModifiers(mods)))
      case p => (p, Tri.U)
    }).toSeq:_*)
  
  protected val rangeCtor: CSet[Property] => Range[this.type]
  protected[prop] final def apply(props: Iterable[Property]) = rangeCtor(props.toSet)
  protected[prop] final def apply(props: Property*) = rangeCtor(props.toSet)
  protected[prop] final def apply(rawFlags:Int) = rangeCtor(toProps(rawFlags, this))
}
object Domain {
  val Class = InnerClass // Any general class
  object ToplevelClass extends Domain(
    Public, Final, Interface, Abstract, Synthetic, Annotation, Enum) {
    protected val rangeCtor = new Range.ToplevelClass(_)
  }
  object InnerClass extends Domain(
    Public, Private, Protected, Static, Final, Interface, Abstract, Synthetic, Annotation, Enum) {
    protected val rangeCtor = new Range.InnerClass(_)
  }
  object Field extends Domain(
    Public, Private, Protected, Static, Final, Volatile, Transient, Synthetic, Enum) {
    protected val rangeCtor = new Range.Field(_)
  }
  object Method extends Domain(
    Public, Private, Protected, Static, Final, Synchronized, Bridge, Varargs, Native, Abstract, Strict, Synthetic, Constructor, DeclaredSynchronized) {
    protected val rangeCtor = new Range.Method(_)
  }
}