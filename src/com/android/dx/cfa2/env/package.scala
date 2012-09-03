package com.android.dx.cfa2

import com.android.dx
import dx.cfa2
import cfa2._
import `var`._
import `val`._

import collection._
import collection.{parallel => par}
//import collection.JavaConversions._

package object env {
  
  trait Env[VarT <: Var_] extends Map[VarT, Val_]
  with PrettyMap[VarT, Val_] with Dumpable with NotNull with Serializable {
    override protected def keyStr(k:VarT) = "[" + k.toString + "]"
    
    def induceUnknowns[E <: Map[VarT, Val_]](prev: E)(implicit cdeps: Val_) : Map[VarT, Val_] = {
      // Gather all seemingly-loop-dependent vals
      val loopDependent: mutable.Set[Val_] = mutable.Set()
      loopDependent ++= (this.keys filter (!prev.contains(_))) map (this(_))
      assert ((prev.keys filter (!this.contains(_))).isEmpty)
      for(v <- this.values)
        if(v.asSet exists (_.dependsUpon(loopDependent, cdeps) != Tri.F))
          loopDependent += v
      val toInduce: mutable.Map[VarT, Val_] = mutable.Map()
      for((k, vs) <- this)
        if(loopDependent contains vs) {
          val newvs = 
            for(v <- vs.asSet) yield
              if(v.isUnknown) v
              else v.typ.unknown
          toInduce += ((k, Val(newvs)))
        }
      toInduce
    }
  }
  
  /** Convenience type; only immutable envs should need this */
  abstract class EnvFactory[VarT <: Var_, E <: Env[VarT] with immutable.MapProxy[VarT,Val_]]
  (proxyCtor: immutable.Map[VarT,Val_] => E)
  extends MapProxyFactory[VarT, Val_, immutable.Map[VarT,Val_], E](immutable.Map(), proxyCtor) {
    def union(a: E, b: E) : E = {
      val build = Builder()
      // Add the unique elements
      build ++= a filterKeys (!b.contains(_))
      build ++= b filterKeys (!a.contains(_))
      // Add the overlapping elements via union
      for(k <- a.keys if b.contains(k);
          (va, vb) = (a(k), b(k)))
        build += ((k, (va union vb)))
      wrap(build result)
    }
  }
  
  type FEnv_ = FEnv[FieldSpec, Var.Field[Instantiable, FieldSpec]]
  type StaticEnv = SFEnv
  
}