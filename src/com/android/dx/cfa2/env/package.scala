package com.android.dx.cfa2

import com.android.dx
import dx.cfa2
import cfa2._
import `var`._
import `val`._

package object env {
  type FEnv_ = FEnv[FieldSpec, Var.Field[Instantiable, FieldSpec]]
}