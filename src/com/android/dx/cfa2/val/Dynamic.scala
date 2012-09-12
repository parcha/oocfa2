package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import env._

import java.lang.reflect
import scala.collection.{parallel=>par, _}

abstract class Dynamic[ET](raw: RawType)(final implicit val EigenType_ : ClassManifest[ET])
extends OBJECT(raw) with Reflected[ET] {
  protected[this] final val constructor = new Instance(_, _)
  
  protected[this] final class Instance_(params: IParams, deps: Val_)
  extends super[OBJECT].Instance_(params, deps) with super[Reflected].Instance_
  with scala.Dynamic { inst:Instance =>
    
    protected[this] final class Ref_(env: HeapEnv) extends super.Ref_(env) { _:Ref =>
    final def \(spec:MethodSpec)(vargs: Val[Reflected[_]]*) : Val_ = {
      val name = spec.getNat.getName.getString
      val proto = spec.getPrototype // WITHOUT the implicit this param
      val argCs =
        for(i <- 0 until proto.getParameterTypes.size;
            val t = proto.getParameterTypes.get(i))
          yield
            Type(t) match {
              case t_ :Reflected[_] => t_.EigenType_.erasure
              case t_ :OBJECT =>
                assert(t_.klass != null)
                t_.klass
            }
      val m = klass.getDeclaredMethod(name, argCs:_*)
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
      
    final def \(m: reflect.Method)(vargs: Val[Reflected[_]]*): Val_ = {
      val retT = Reflected(m.getReturnType).get
      
      type Args = Seq[VAL[Reflected[_]]]
      def f(_args: Args) : VAL[retT.type] = {
        if(_args exists (_.isUnknown)) return retT unknown Val((_args :+ inst):_*)
        type Reflected_ = Reflected[_]
        val args = _args.asInstanceOf[Seq[Reflected_ #Instance]]
        
        val args_ = for(v <- args) yield v.asInstanceOf[v.typ.Instance].self.asInstanceOf[Object]
        val hash = self.hashCode
        val ret = m.invoke(self, args_ :_*).asInstanceOf[retT.EigenType]
        // Assert that we didn't mutate the lifted reference
        assert(self.hashCode == hash)
        val deps = Val((args :+ inst):_*)
        return retT instance(ret, deps)
      }
      
      Val.eval (f(_:Args)) (vargs:_*)
    }
    }
    final type Ref = Ref_
    protected[this] final val ref = new Ref(_)
  }
  final type Instance = Instance_
}
object Dynamic {
  /** Check if an instance method is liftable given an instance and args */
  def isLiftableCall(spec:MethodSpec, vobj:Val_, vargs:Val_ *) = {
    isLiftableMethod(spec) &&
    !(vobj satisfies (_.isUnknown)) &&
    (vargs forall (_.asSet forall (_.typ.isInstanceOf[Reflected[_]])))
  }
  /** Check if a static method is liftable given args */
  def isLiftableStaticCall(spec:MethodSpec, vargs:Val_ *) = {
    isLiftableMethod(spec) &&
    (vargs forall (_.asSet forall (_.typ.isInstanceOf[Reflected[_]])))
  }
  
  @inline
  def isLiftableMethod(spec: MethodSpec) = {
    val typ = Type(spec.getDefiningClass.getClassType).asInstanceOf[OBJECT]
    val retT = Type(spec.getPrototype.getReturnType)
    typ.isInstanceOf[Dynamic[_]] &&
    retT.isInstanceOf[Reflected[_]]
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