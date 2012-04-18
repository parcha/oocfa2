package com.android.dx.cfa2.tlc

sealed trait CMP {
  import BOOL._
  
  type ~[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up
	
  type >  = ~[False, False, True, BOOL]
  type >= = ~[False, True, True, BOOL]
  type == = ~[False, True, False, BOOL]
  type <= = ~[True, True, False, BOOL]
  type <  = ~[True, False, False, BOOL]
}
object CMP {
  
  sealed trait GT extends CMP {
	type ~[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
  }
  sealed trait LT extends CMP {
	type ~[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
  }
  sealed trait EQ extends CMP {
	type ~[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
  }

}