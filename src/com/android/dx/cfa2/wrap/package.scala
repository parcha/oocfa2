package com.android.dx.cfa2

package object wrap {
  val MethodDesc = _MethodDesc
  type MethodDesc = _MethodDesc[Self forSome { type Self <: _MethodDesc[Self] }]
  type DalvikMethodDesc = _DalvikMethodDesc[Self forSome { type Self <: _DalvikMethodDesc[Self] }]
  type ReflMethodDesc = _ReflMethodDesc[Self forSome { type Self <: _ReflMethodDesc[Self] }]
  
  val ClassDesc = _ClassDesc
  type ClassDesc = _ClassDesc[Self forSome { type Self <: _ClassDesc[Self] }]
  type DalvikClassDesc = _DalvikClassDesc[Self forSome { type Self <: _DalvikClassDesc[Self] }]
  
  type FieldSlotDesc = _FieldSlotDesc[Self forSome { type Self <: _FieldSlotDesc[Self] }]
  
  trait StrawmanOrdering[T] extends java.util.Comparator[T] with Ordering[T] {
    implicit def strawmanOrdering[T_ <: T] = this.asInstanceOf[Ordering[T_]]
  }
}