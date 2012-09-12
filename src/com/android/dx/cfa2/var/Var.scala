package com.android.dx.cfa2.`var`

import com.android.dx
import dx.cfa2
import cfa2._
import time._
import `val`._
import prop._
import tlc.Algebra._

/**
 * Leaf classes should be case classes based on src (or its components) such that
 * two Vars with the same src are considered the same.
 */
sealed abstract class Var[+T <: Instantiable] extends Immutable with NotNull with Serializable {
  type Source
  val src: Source
  type VT = T
  // FIXME: src can be null?!?!
  override def toString = if(src != null) src toString else ""
  override def equals(that: Any) = that match {
    case that:Var[T] => that.src == this.src
    case _ => super.equals(that)
  }
  override lazy val hashCode = src hashCode
}
object Var {
    //type NonHeap[+T <: Instantiable] = UNION [Register[T]]# | [Stack[T]]# ^
    //type NonHeap_ = NonHeap[Instantiable]
    
    /** From a register; used for vars between BBs and functions */
    final case class Register[+T <: Instantiable](val src: Reg) extends Var[T] {
      type Source = Reg
      def typ : Instantiable = Type(src.getType).asInstanceOf[Instantiable]
      
      // HACK: RegisterSpecs normally depend on their types as well, which would make them falsely unequal @_@
      override def equals(that: Any) = that match {
        case that:Register[T] => that.src.getReg == this.src.getReg
        case _ => super.equals(that)
      }
      override lazy val hashCode = src.getReg
    }
    type Register_ = Register[Instantiable]
    
    /** Off the stack; used for function params */
    /*final case class Stack[+T <: Instantiable](val src: Method) extends Var[T] {
      type Source = Method
      val depth: Int
    }
    type Stack_ = Stack[Instantiable]*/
    
    sealed abstract class Heap[+T <: Instantiable] extends Var[T] /*with Timekeeper*/ {
      //override def toString = src + "%" + access + ":" + super.toString
      //val access : Properties.Access
    }
    type Heap_ = Heap[Instantiable]
    object Heap {
      sealed trait Mutable[+T <: Instantiable] extends Heap[T] {
        type Timestamp <: LinearTime
      }
      type Mutable_ = Mutable[Instantiable]
      sealed trait Immutable[+T <: Instantiable] extends Heap[T] {
        type Timestamp <: UnitTime
      }
      type Immutable_ = Immutable[Instantiable]
    }
    
    /** Indexed by heap-tokens */
    abstract case class RawHeap[+T <: RefType](final val heapToken: Long) extends Heap[T] {
      type Source = Long
      final val src = heapToken
    }
    type RawHeap_ = RawHeap[RefType]
    
    /** Field of an object (be it a class or an instance) */
    sealed abstract class Field[+T <: Instantiable, Spec <: FieldSpec] (val spec: Spec)
    extends Heap[T]
    type Field_ = Field[Instantiable, FieldSpec]
    
    /** Static class var */
    sealed abstract case class StaticF[+T <: Instantiable]
    (final val src: SFieldSpec)
    extends Field[T, SFieldSpec](src) {
      type Source = SFieldSpec
    }
    type StaticF_ = StaticF[Instantiable]
    
    final class MStaticF[+T <: Instantiable](spec: SFieldSpec)
    extends StaticF[T](spec) with Heap.Mutable[T] {
      //val timestamp = new LinearTime
    }
    type MStaticF_ = MStaticF[Instantiable]
    final class IStaticF[T <: Instantiable](spec: SFieldSpec)
    extends StaticF[T](spec) with Heap.Immutable[T] {
      //val timestamp = new UnitTime
    }
    type IStaticF_ = IStaticF[Instantiable]
    
    /** Field of an object */
    sealed abstract case class InstanceF[+T <: Instantiable]
    (final val obj: SUBV[OBJECT], final override val spec: IFieldSpec)
    extends Field[T, IFieldSpec](spec) {
      type Source = (VAL[OBJECT], IFieldSpec)
      final val src = (obj, spec)
      type Timestamp <: HierarchicalTime
    }
    type InstanceF_ = InstanceF[Instantiable]
    
    final class MInstanceF[+T <: Instantiable](obj: SUBV[OBJECT], spec: IFieldSpec)
    extends InstanceF(obj, spec) with Heap.Mutable[T] {
      type Timestamp = HLinearTime
      //val timestamp = new HLinearTime(src.timestamp)
    }
    type MInstanceF_ = MInstanceF[Instantiable]
    final class IInstanceF[+T <: Instantiable](obj: SUBV[OBJECT], spec: IFieldSpec)
    extends InstanceF(obj, spec) with Heap.Immutable[T] {
      type Timestamp = HUnitTime
      //val timestamp = new HUnitTime(src.timestamp)
    }
    type IInstanceF_ = IInstanceF[Instantiable]
  }
