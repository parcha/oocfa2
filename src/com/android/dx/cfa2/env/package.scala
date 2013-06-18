package com.android.dx.cfa2

import `var`._
import `val`._

package object env {
  type FEnv_ = FEnv[FieldSpec, Var.Field[Instantiable, FieldSpec]]
  
  val Heap = _Heap
  type Heap[Scheme <: HeapScheme] = Self forSome { type Self <: _Heap[Self, Scheme]}
}