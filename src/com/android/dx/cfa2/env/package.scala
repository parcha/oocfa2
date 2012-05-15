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
    
    def induceUnknowns[E <: Map[VarT, Val_]](prev: E) : Map[VarT, Val_] = {
      // Gather all seemingly-loop-dependent vals
      val loopDependent: mutable.Set[Val_] = mutable.Set()
      loopDependent ++= (this.keys filter (!prev.contains(_))) map (this(_))
      assert ((prev.keys filter (!this.contains(_))).isEmpty)
      for(v <- this.values)
        if(v.asSet exists (_.dependsUpon(loopDependent) != Tri.F))
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
  
  /** Either an (immutable) environment or its builder */
  /*sealed trait EnvLike[VarT <: Var_] {
    type Self <: EnvLike[VarT]
    def + (kv: (VarT, Val_)) : Self
  }*/
  
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
  
  /**
   * Composite environments which are actually composed of several environments
   * and delegate lookup among them. They must be immutable because they're obviously not unitary.
   */
  sealed trait Composite[VarT <: Var_, +E <: Env[VarT]]
  extends Env[VarT] with immutable.Map[VarT, Val_]
  sealed trait CompositeLike[VarT <: Var_, +E <: Env[VarT]] /*extends EnvLike[VarT]*/ {
    //def + (env: E) : Self
  }
  
  /**
   * Linked environments which are composed of a linked chain of environments.
   * It's intended to be a faux kind, thus "This" is intended to be a "top type instance" of it. 
   */
  sealed trait Linked[VarT <: Var_, +This <: Linked[VarT, This]] extends Composite[VarT, This] {
    val parent: This
    final abstract override def get(key:VarT) : Option[Val_] =
      super.get(key) match {
        case Some(value) => Some(value)
        case None        => if(parent == null) None else parent get key
    }
  }
  
  /**
   * Linked environments with multiple parents
   */
  //sealed trait MultiLinked[VarT <: Var_, +P <: Map[VarT, Val_]] extends Composite[VarT]
  
  /**
   *  Stacked environments which are composed of several environments.
   *  Compared to a Linked environment, a Stacked environment is an /external/ linked list,
   *  whereas a Linked environment is an /intrusive/ linked list.
   */
  /*sealed trait StackedLike[VarT <: Var_, +E <: Env[VarT]]
  extends CompositeLike[VarT, E] {
    type Self <: StackedLike[VarT, E]
    def + (e:E) : Self
  }
  sealed trait Stacked[VarT <: Var_, +E <: Env[VarT]] extends Composite[VarT, E] {
    val subenvs: Seq[E]
    def get [SubVar <: VarT]
            (key: SubVar) : Option[Val_] =
      (subenvs.reduceLeft ((b, a) => (b orElse a).asInstanceOf[E]) lift)(key) 
      
    override def iterator = throw new UnsupportedOperationException
    override def + [B>:Val_] (kv: (VarT, B)) = throw new UnsupportedOperationException
    override def - (k: VarT) = throw new UnsupportedOperationException
    
    def contains(env: E) = subenvs.contains(env)
  }
  object Stacked {
    sealed abstract class Builder[VarT <: Var_, +E <: Env[VarT], S <: Stacked[VarT,E]] protected
                          (start: S = null)
    extends mutable.Builder[E, S] with Stacked[VarT, E] with CompositeLike[VarT, E] {
      protected val builder = mutable.LinkedList[E]()
      final val subenvs = builder
      final def += (env: E) = { builder :+ env; this; }
      final def clear = { builder; this }
      final def result =
        if(start == null) result_
        else if(builder isEmpty) start
        else (start ++ result_).asInstanceOf[S]
      protected def result_ : S
      
      type Self = this.type
      final def + (e: E) = { this += e }
    }
  }*/
  
  type FEnv_ = FEnv[FieldSpec, Var.Field[Instantiable, FieldSpec]]
  type StaticEnv = SFEnv
  
  /*sealed trait StaticEnvLike extends Env[Var.StaticF_] with EnvLike[Var.StaticF_]
  /** The global environment; holds static vars from different classes */
  final class StaticEnv
  extends immutable.HashMap[Var.StaticF_, Val_] with StaticEnvLike {
    type Self = StaticEnv
  }
  object StaticEnv {
    final class Builder(start: StaticEnv = new StaticEnv)
    extends mutable.MapBuilder[Var.StaticF_, Val_, StaticEnv](start) with StaticEnvLike {
      type Self = Builder
    }
  }*/
  //sealed trait FrameLike extends Env[Var.Register_]
  
  /*sealed trait BBFrameLike extends FrameLike
  /** The unitary environment for a BB */
  sealed class BBFrame
  extends immutable.HashMap[Var.Register_, Val_] with BBFrameLike
  object BBFrame extends EnvFactory[Var.Register_, BBFrame](new BBFrame)*/

  // The special environment at the beginning of a function
  /*final class FuncEntry protected() extends BBFrame_
  object FuncEntry {
    def apply[T <: Type](vars: (Var.Register, Val)*) = {
      val build = new Builder
      build ++= vars
      build result
    }
    // Helper for building up a BBFrame
	final class Builder
	extends mutable.MapBuilder[Var.Register, Val, FuncEntry](new FuncEntry)
	with Frame
  }*/
  
  /*sealed trait EBBFrameLike
  extends FrameLike with CompositeLike[Var.Register_, EBBFrameLike]
  /** The environment of an EBB */
  final class EBBFrame (subframes: Seq[BBFrame])
  extends immutable.HashMap[Var.Register_, Val_]
  with Stacked[Var.Register_, BBFrame] with EBBFrameLike {
    val subenvs = subframes
  }
  object EBBFrame {
    def apply[T <: Type](subframes: BBFrame*) = {
      val build = new Builder
      for(sub <- subframes)
    	build += sub
      build result
    }
	final class Builder (start: EBBFrame = null)
	extends Stacked.Builder[Var.Register_, BBFrame, EBBFrame](start) with EBBFrameLike {
	  def result_ = new EBBFrame(builder)
	}
  }*/
  
  /*sealed trait TraceEnvLike extends Env[Var.Register_]
  /** The environment of a trace through a method */
  final class TraceEnv (subframes: Seq[EBBFrame])
  extends immutable.HashMap[Var.Register_, Val_]
  with Stacked[Var.Register_, EBBFrame] with TraceEnvLike {
    val subenvs = subframes
  }
  object TraceEnv {
    final class Builder (start: TraceEnv = null)
    extends Stacked.Builder[Var.Register_, EBBFrame, TraceEnv](start) with TraceEnvLike {
      def result_ = new TraceEnv(builder)
    }
  }*/
  
  /*/**
   *  The environment of an object (its fields). It is linked such that an object O
   *  can look up its fields as it views itself as supertypes.
   *  It's meant to always be subclassed; that is, its type hierarchy should eventually lead to
   *  an object (in the Scala sense) or anonymous class. 
   */
  final class OEnv (val parent:OEnv)
  extends immutable.HashMap[Var.Field_, Val_] with Linked[Var.Field_, OEnv]*/
  /*object OEnv {
    // Helper for building up an OEnv
    final class Builder
    extends mutable.MapBuilder[Var.Field_, Val_, OEnv](new OEnv)
    with Env[Var.Field]
  }*/
  
}