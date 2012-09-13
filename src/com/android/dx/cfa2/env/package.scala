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
  // TODO: PrettyMap takes up too much space at the moment
  /*with PrettyMap[VarT, Val_]*/ with Dumpable with NotNull with Serializable {
    // FIXME: This may be wrong... or at least not precise enough. Also consider heap vs. static-env
    def induceUnknowns[E <: Map[VarT, Val_]](prev: E)(implicit cdeps: Val_) : Map[VarT, Val_] = {
      if(prev.isEmpty) return this
      // Gather all seemingly-loop-dependent vals
      val loopDependent: mutable.Set[Val_] = mutable.Set()
      loopDependent ++= (this.keys filter (!prev.contains(_))) map (this(_))
      // The keys from the previous environment should still exist in this one
      /*assert ((prev.keys filterNot (this contains _)).isEmpty,
              (prev.keys filterNot (this contains _)))*/
      // FIXME: Just log the assertion for now
      if(!(prev.keys filterNot (this contains _)).isEmpty)
        CFA2Analysis.log('debug) ("Inconsistent environments when inducing unknowns:\n"+
                                  (prev.keys filterNot (this contains _)))
      for(v <- this.values)
        if(v.asSet exists (_.dependsUpon(loopDependent ++ immutable.Set(cdeps)) != Tri.F))
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
      return toInduce
    }
  }
  
  /** Convenience type; only immutable envs should need this */
  abstract class EnvFactory[VarT <: Var_, E <: Env[VarT] with immutable.MapProxy[VarT,Val_]]
  (proxyCtor: immutable.Map[VarT,Val_] => E)
  extends MapProxyFactory[VarT, Val_, immutable.Map[VarT,Val_], E](immutable.Map(), proxyCtor) {
    final val defaultM: M = immutable.Map()
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