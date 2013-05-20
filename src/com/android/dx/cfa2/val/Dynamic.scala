package com.android.dx.cfa2.`val`

import language.dynamics
import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import wrap._
import env._
import java.lang.reflect
import scala.collection.{parallel=>par, _}
import scala.reflect.ClassTag
import com.android.dx.cfa2.analysis.CFA2Analysis

abstract class Dynamic[ET](raw: RawType)(final implicit val EigenType_ : ClassTag[ET])
extends OBJECT(raw) with Reflected[ET] {
  import Dynamic._
  
  protected[this] final val constructor = new Instance(_, _)
  
  protected[this] final class Instance_(params: IParams, deps: Val_)
  extends super[OBJECT].Instance_(params, deps) with super[Reflected].Instance_ { inst:Instance =>
    
    protected[this] final class Ref_(env: HeapEnv) extends super.Ref_(env) { _:Ref =>
    final def \(mdesc:MethodDesc)(vargs: Val[Reflected[_]]*) : Val_ = {
      val m = klass.getDeclaredMethod(mdesc.name, mdesc.argCs.get:_*)
      (this\m)(vargs:_*)
    }
    
    final def \(name:String)(vargs: Val[Reflected[_]]*) : Val_ = {
      //require(!this.isUnknown)
      val argCs = vargs map (_.asSet.head.typ.EigenType_.erasure)
      val m = klass.getDeclaredMethod(name, argCs:_*)
      (this\m)(vargs:_*)
    }
    final def applyDynamic(name: String, args: Any*) =
      (this\name)(args.asInstanceOf[Seq[Val[Reflected[_]]]]:_*)
      
    private[this] final def \(m: reflect.Method)(vargs: Val[Reflected[_]]*): Val[Reflected[_]] = {
      type Args = Seq[VAL[Reflected[_]]]
      val f = liftCall(false, m, vargs:_*)
      def g(_args: Args) = {
        val hash = self.hashCode
        val ret = f(_args :+ inst)
        // Assert that we didn't mutate the lifted reference
        assert(self.hashCode == hash)
        ret
      }
      if(vargs.isEmpty)
        Val.Atom(g(Seq()))
      else
        Val.eval(g)(vargs:_*)
    }
    }
    final type Ref = Ref_
    protected[this] final val ref = new Ref(_)
  }
  final type Instance = Instance_
}
object Dynamic {
  /** Check if an instance method is liftable given an instance and args */
  def isLiftableCall(mdesc:MethodDesc, vobj:Val_, vargs:Val_ *) = {
    mdesc.isLiftableI &&
    !(vobj satisfies (_.isUnknown)) &&
    (vargs forall (_.asSet forall (_.typ.isInstanceOf[Reflected[_]])))
  }
  /** Check if a static method is liftable given args */
  def isLiftableStaticCall(mdesc:MethodDesc, vargs:Val_ *) = {
    mdesc.isLiftableS &&
    (vargs forall (_.asSet forall (_.typ.isInstanceOf[Reflected[_]])))
  }
  
  def liftStaticCall(mdesc:MethodDesc, _vargs:Val_ *): Val[Reflected[_]] = {
    assert(isLiftableStaticCall(mdesc, _vargs:_*))
    val vargs = _vargs.asInstanceOf[Seq[Val[Reflected[_]]]]
    val f = liftCall(true, mdesc.reflection.get, vargs:_*)
    Val.eval(f)(vargs:_*)
  }
  
  private def liftCall(isStatic: Boolean, m: reflect.Method, vargs: Val[Reflected[_]]*)
              : Seq[VAL[Reflected[_]]] => VAL[Reflected[_]] = {
    val retT = Reflected(m.getReturnType).get
    type Args = Seq[VAL[Reflected[_]]]
    def f(_args: Args) : VAL[retT.type] = {
      val args = _args.asInstanceOf[Seq[INST[Reflected_]]]
      val deps = Val(args:_*)
      lazy val unknown = retT unknown deps 
      if(_args exists (_.isUnknown))
        return unknown
      val args_ = for(v <- args) yield v.asInstanceOf[v.typ.Instance].self.asInstanceOf[Object]
      try {
        val ret: retT.EigenType =
          if(isStatic)
            m.invoke(null, args_ :_*).asInstanceOf[retT.EigenType]
          else
            m.invoke(args_.head, args_.tail :_*).asInstanceOf[retT.EigenType]
        return retT instance(ret, deps)
      }
      catch {
        case e:IllegalArgumentException =>
          CFA2Analysis.log('warn) (s"IllegalArgumentException when trying to lift call on $m using $vargs")
          e.printStackTrace(CFA2Analysis.logs('warn).stream)
          return unknown
      }
    }
    return f
  }
  
  val smeth_whitelist = {
    val builder = par.immutable.ParSeq.newBuilder[reflect.Method]
    // String.valueOf
    import java.{lang=>J}
    val str = classOf[J.String]
    builder += str.getMethod("valueOf", classOf[Boolean])
    builder += str.getMethod("valueOf", classOf[Char])
    builder += str.getMethod("valueOf", classOf[Double])
    builder += str.getMethod("valueOf", classOf[Float])
    builder += str.getMethod("valueOf", classOf[Int])
    builder += str.getMethod("valueOf", classOf[Long])
    builder.result
  }
}