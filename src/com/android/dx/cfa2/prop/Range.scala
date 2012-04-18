package com.android.dx.cfa2.prop

import com.android.dx
import dx.cfa2._
import prop.Properties._
import scala.collection.{Set => CSet}

sealed class Range[+D <: Domain] protected (dom:D, props: CSet[Property]) extends Set(props) {
  require(props.toSet subsetOf dom)
}
object Range {
  final case class Class      (props: CSet[Property]) extends Range(Domain.Class, props)
  final case class InnerClass (props: CSet[Property]) extends Range(Domain.InnerClass, props)
  final case class Field      (props: CSet[Property]) extends Range(Domain.Field, props)
  final case class Method     (props: CSet[Property]) extends Range(Domain.Method, props)
  
  def apply[D <: Domain](dom:D, rawFlags:Int)             : Range[D] = dom(rawFlags)
  def apply[D <: Domain](dom:D, props:Iterable[Property]) : Range[D] = dom(props)
  def apply[D <: Domain](dom:D, props:Property*)          : Range[D] = dom(props)
}