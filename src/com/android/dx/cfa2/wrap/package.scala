package com.android.dx.cfa2

import com.android.dx.cfa2
import cfa2.wrap._

package object wrap {
  val MethodDesc = _MethodDesc
  type MethodDesc = _MethodDesc[Self] forSome { type Self <: _MethodDesc[Self] }
  type DalvikMethodDesc = _DalvikMethodDesc[Self] forSome { type Self <: _DalvikMethodDesc[Self] }
  type ReflMethodDesc = _ReflMethodDesc[Self] forSome { type Self <: _ReflMethodDesc[Self] }
  
  val ClassDesc = _ClassDesc
  type ClassDesc = _ClassDesc[Self] forSome { type Self <: _ClassDesc[Self] }
  type DalvikClassDesc = _DalvikClassDesc[Self] forSome { type Self <: _DalvikClassDesc[Self] }
  
  type FieldSlotDesc = _FieldSlotDesc[Self] forSome { type Self <: _FieldSlotDesc[Self] }
}