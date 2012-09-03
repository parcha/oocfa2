package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import cfa2._
import tlc._
//import tlc.Algebra._
import tlc.Func._

import scala.reflect.Manifest
import scala.annotation.unchecked._
import scala.collection.{Set => CSet, Seq => CSeq, _}
import immutable._
import parallel.immutable._

sealed abstract class Val[+T <: Instantiable]
extends collection.SetProxy[VAL[T @uncheckedVariance]] with Immutable with Serializable with NotNull{
  import Val._
  // TODO: Allow us to infer/capture the LUB of the typs of these vals
  final type Type = T
  /** The type of functions taking these VALs and returning R */
  final type F[R] = VAL[T] => R
  
  // Set proxy
  final def self: CSet[VAL[T @uncheckedVariance]] = asSet.seq
  val asSet: GenSet[VAL[T @uncheckedVariance]]
  
  override lazy val size = super.size  
  
  final def union[T <: Instantiable](that:Val[T]) =
    Val.union(this, that)
  
  /*final override def equals(that) = that match {
    case that:Val[T] => equals(this, that)
    case _ => false
  }*/
  
  final def eval [ArgT >: Type <: Instantiable, RetT <: Instantiable]
                 (f: VAL[ArgT]=>VAL[RetT]) : Val[RetT] = {
    val rets = for(arg <- asSet) yield f(arg)
    Val(rets)
  }
  final def eval_ [ArgT >: Type <: Instantiable, RetT <: Instantiable]
                  (f: VAL[ArgT]=>Val[RetT]) : Val[RetT] = {
    val rets = for(arg <- asSet) yield f(arg)
    if(rets isEmpty)
      Bottom
    else
      rets reduce Val.union[RetT, RetT, RetT]
  }
  
  /*final def invoke(name:String, args:Val_*) : Val_ = {
    
  }*/
  
  final def satisfies (test: VAL[T] => Boolean) = asSet exists {test(_)}
  
  //final def apply[R] (f: VAL[T] => R): immutable.Set[R] = asSet 
}
object Val {
  def apply[Join <: Instantiable]
           (vs: VAL[Join]*) : Val[Join] = apply(vs.toSet)
  def apply[Join <: Instantiable]
           (vs: GenSet[VAL[Join]]) : Val[Join] = {
    val vs_ = {
      // Only keep values of a type which is not superseded by an unknown of a subtype
      val superceded = mutable.Set[VAL[Join]]()
      for(u <- vs if u.isUnknown)
        superceded ++= vs.seq filter ((v: VAL[Join]) =>
          if((v.typ < u.typ)==Tri.T)
            if(v.isUnknown)
              if(u == v) false
              else (v.typ > u.typ) match {
                case Tri.T => !superceded.contains(u) // occurs check
                case Tri.U => false // Be conservative
                case Tri.F => true // There is no cycle; u is strictly >
              }
            else true
          else false)
      val chk = vs diff superceded
      assert(!chk.isEmpty)
      chk
    }
    vs_.size match {
      case 0 => Bottom
      case 1 => Atom(vs_.iterator.next)
      //case 2 => XOr[Join, Join, Join](vs_(0), vs_(1))
      case _ => new UnionSet(vs_)
    }
  }
  
  def unapplySeq[T <: Instantiable](v: Val[T]) : Seq[VAL[T]] = v match {
    case Bottom            => Seq()
    // TODO
    case Top               => Seq()
    //case Unknown(u)        => Seq(u)
    case Atom(v_)          => Seq(v_)
    case UnionSet(vs @ _*) => Seq(vs:_*)
  }
  
  /*def union[T1 <: Instantiable, T2 <: Instantiable,
            Join : Join2[T1, T2, Instantiable]# ^]
           (v1: Val[T1], v2:Val[T2]) : Val[Join] = {
    val seq = (unapplySeq(v1) :+ unapplySeq(v2)).asInstanceOf[Seq[VAL[Join]]]
    apply(seq:_*)
  }*/
  
  def union[T1 <: Join, T2 <: Join, Join <: Instantiable]
           (v1: Val[T1], v2:Val[T2]) : Val[Join] = {
    if(v1 == Val.Top || v2 == Val.Top)
      Val.Top
    else {
      val u = v1.asSet ++ v2.asSet
      apply(u)
    }
  }
  
  def deepUnion[Join <: Instantiable]
               (vs: GenIterable[Val[Join]]) : Val[Join] =  vs.size match {
    case 0 => Bottom
    case 1 => vs head
    case _ => vs reduce (union(_, _))
  }
    
  //TODO
  /*def equals[T1 <: Join, T2 <: Join, Join <: Instantiable]
            (v1: Val[T1], v2: Val[T2]) : Boolean = {
    val s1 = unapplySeq(v1)
    val s2 = unapplySeq(v2)
    if(s1.size != s2.size) false
    else {
      s1.
    }
  }*/
  
  def eval[ArgT <: Instantiable, RetT <: Instantiable]
          (f: Seq[VAL[ArgT]]=>VAL[RetT])(vargs: Val[ArgT]*) : Val[RetT] = {
    val rets = for(poss <- mkPossibilities(vargs)) yield f(poss)
    Val(rets)
  }
  def eval_[ArgT <: Instantiable, RetT <: Instantiable]
           (f: Seq[VAL[ArgT]]=>Val[RetT])(vargs: Val[ArgT]*) : Val[RetT] = {
    val rets = for(poss <- mkPossibilities(vargs)) yield f(poss)
    Val.deepUnion(rets)
  }
  @inline
  private def mkPossibilities[ArgT <: Instantiable](vargs: CSeq[Val[ArgT]]) = {
    val argSets = for(arg <- vargs) yield Val.unapplySeq(arg)
    // Combinatorial set of all combinations of all arguments from each argSet
    val possibilities: mutable.Set[Seq[VAL[ArgT]]] = mutable.Set()
    def comb(ss :CSeq[Seq[VAL[ArgT]]], acc: immutable.Seq[VAL[ArgT]] = immutable.Seq()) : Unit = {
      val (h, t) = (ss head, ss tail)
      for(arg <- h) {
        val n = acc :+ arg
        if(!t.isEmpty) comb(t, n)
        else possibilities += n
      }
    }
    comb(argSets)
    possibilities
  }
  
  case object Bottom extends Val[Nothing] {
    val asSet = Set[VAL[Nothing]]()
  }
  // TODO: How can we get this to actually be Top?
  case object Top extends Val[Nothing] {
    val asSet = Set[VAL[Nothing]]()
  }
  
  /** Special "subtype" just for checking if a value is unknown */
  object Unknown /*extends Atom[Nothing](null)*/ {
    def apply[T <: Instantiable](typ:T) = Atom(typ.unknown)
    def unapply[T <: Instantiable](atom:Atom[T]) : Option[VAL[T]] =
      if(atom.v.isUnknown) Some(atom.v)
      else None
  }
  
  sealed case class Atom [+T <: Instantiable] (v:VAL[T]) extends Val[T] {
    val asSet: Set[VAL[T @uncheckedVariance]] = Set(v)
  }
  type Atom_ = Atom[Instantiable]
  
  /*final case class XOr [+T1 <: Instantiable, +T2 <: Instantiable,
                        +Join : Join2[T1, T2, Instantiable]# ^]
                        (v1:VAL[T1], v2:VAL[T2])
  extends Val[Join]*/
  
  final class UnionSet[Join <: Instantiable] private[Val]
                      (vs: GenSet[SUBV[SUBT[Join]]]) //(implicit Join_ : Manifest[Join])
  extends Val[Join] {
    override lazy val asSet = vs
  }
  type UnionSet_ = UnionSet[Instantiable]
  object UnionSet {
    def apply[J <: Instantiable](vs: SUBV[SUBT[J]]*) = new UnionSet[J](Set(vs:_*))
    def unapplySeq[J <: Instantiable](u:UnionSet[J]) : Option[CSeq[SUBV[J]]] = Some(u.toSeq)
  }
}