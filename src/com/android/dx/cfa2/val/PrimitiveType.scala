package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}

sealed trait PrimitiveType extends Instantiable {
  require(raw isPrimitive)
  protected final override def << (t: Type) = (super.<<(t)) | false
  protected final override def >> (t: Type) = (super.>>(t)) | false
}

object VOID extends Instantiable(RawType.VOID) with PrimitiveType with Singleton {
  override lazy val singleton = Instance_
  protected[this] object Instance_ extends super.Instance_(paramify(), Val.Bottom)
  type Instance = Instance_
}

sealed abstract class ValuedType[V <: AnyVal] protected[`val`] (raw:RawType)(implicit val EigenType_ : ClassManifest[V])
extends Instantiable(raw) with PrimitiveType with Reflected[V] with Type.CanBeParam {
  final override def instance(deps: Val_ =Val.Bottom, params: IParams=paramify()) = {
    require(params contains 'self, params.size==1)
    cacheHook(params('self).asInstanceOf[V], deps) match {
      case None        => constructor(params, deps)
      case Some(cache) => cache 
    }
  }
  /** Subclasses should override if they want to cache certain values */
  protected[this] def cacheHook(self:V, deps: Val_) : Option[Instance] = None
  
  protected[this] final val constructor = new Instance(_, _)
  protected[this] final class Instance_(params: IParams, deps:Val_)
  extends super[PrimitiveType].Instance_(params, deps) with super[Reflected].Instance_
  final type Instance = Instance_
}
object BOOLEAN extends ValuedType[Boolean](RawType.BOOLEAN) {
  //protected val _V = manifest[Boolean].erasure
  protected def true_(deps: Val_) = instance(true, deps)
  protected def false_(deps: Val_) = instance(false, deps)
  val TRUE  = true_(Val.Bottom)
  val FALSE = false_(Val.Bottom)
  protected[this] override def cacheHook(self:Boolean, deps: Val_)  =
    if(deps != Val.Bottom) None
    else self match {
      case true  => Some(TRUE)
      case false => Some(FALSE)
    }
  implicit def asInt(inst:Instance) = if(inst.self) 1 else 0
}
object BYTE extends ValuedType[Byte](RawType.BYTE) with Type.Integral {
  //protected val _V = manifest[Byte]
  implicit val lifter = scala.Numeric.ByteIsIntegral
}
object CHAR extends ValuedType[Char](RawType.CHAR) with Type.Integral {
  //protected val _V = manifest[Char]
  implicit val lifter = scala.Numeric.CharIsIntegral
}
object DOUBLE extends ValuedType[Double](RawType.DOUBLE) with Type.Fractional {
  //protected val _V = manifest[Double]
  implicit val lifter = scala.Numeric.DoubleIsFractional
}
object FLOAT extends ValuedType[Float](RawType.FLOAT) with Type.Fractional {
  //protected val _V = manifest[Float]
  implicit val lifter = scala.Numeric.FloatIsFractional
}
object INT extends ValuedType[Int](RawType.INT) with Type.Integral {
  //protected val _V = manifest[Int]
  implicit val lifter = scala.Numeric.IntIsIntegral
  implicit def asBoolean(inst:Instance) = inst.self!=0
  val ZERO = instance(0, Val.Bottom)
  val ONE  = instance(1, Val.Bottom)
  protected[this] override def cacheHook(self:Int, deps: Val_) =
    if(deps != Val.Bottom) None
    else self match {
      case 0 => Some(ZERO)
      case 1 => Some(ONE)
      case _ => None
    }
}
object LONG extends ValuedType[Long](RawType.LONG) with Type.Integral {
  //protected val _V = manifest[Long]
  implicit val lifter = scala.Numeric.LongIsIntegral
}
object SHORT extends ValuedType[Short](RawType.SHORT) with Type.Integral {
  //protected val _V = manifest[Short]
  implicit val lifter = scala.Numeric.ShortIsIntegral
}