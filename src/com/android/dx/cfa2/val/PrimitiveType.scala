package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import scala.reflect.{ClassTag, classTag}

sealed trait PrimitiveType extends Instantiable {
  require(raw isPrimitive)
  protected final override def << (t: Type) = (super.<<(t)) | false
  protected final override def >> (t: Type) = (super.>>(t)) | false
}

object VOID extends Instantiable(RawType.VOID) with PrimitiveType with Singleton {
  val klass = null
  val defaultInst = null
  override lazy val singleton = Instance_
  protected[this] object Instance_ extends super.Instance_(paramify(), Val.Bottom)
  type Instance = Instance_
}

sealed abstract class ValuedType[V <: AnyVal] protected[`val`] (raw:RawType)(implicit val EigenType_ : ClassTag[V])
extends Instantiable(raw) with PrimitiveType with Reflected[V] with Type.CanBeParam {
  protected[this] val default: V
  val defaultInst = instance(Val.Bottom, paramify(('self, default)))
  final override def instance(deps: Val_, params: IParams) = {
    require(params contains 'self, params.size==1)
    cacheHook(params('self).asInstanceOf[V], deps) match {
      case None        => constructor(params, deps)
      case Some(cache) => cache 
    }
  }
  /** Subclasses should override this partial function if they want to cache certain values */
  protected[this] val cacheHook: PartialFunction[V, Instance] = null
  /** Subclasses should override if they want to cache certain values, even with deps */
  protected[this] def cacheHook(self:V, deps: Val_) : Option[Instance] =
    if(deps != Val.Bottom) None // Only cache fresh values
    else if(self == default) Some(defaultInst) // Always "cache" the default value
    else if(cacheHook != null) (cacheHook.lift)(self)
    else None
    
  protected[this] final val constructor = new Instance(_, _)
  protected[this] final class Instance_(params: IParams, deps:Val_)
  extends super[PrimitiveType].Instance_(params, deps) with super[Reflected].Instance_
  final type Instance = Instance_
}
object BOOLEAN extends ValuedType[Boolean](RawType.BOOLEAN) {
  //protected val _V = manifest[Boolean].erasure
  protected[this] val default = false
  protected def true_(deps: Val_) = instance(true, deps)
  protected def false_(deps: Val_) = instance(false, deps)
  val TRUE  = true_(Val.Bottom)
  val FALSE = defaultInst
  override protected[this] def cacheHook = {
    case true  => TRUE
    case false => FALSE
  }
  implicit def asInt(inst:Instance) = if(inst.self) 1 else 0
}
object BYTE extends ValuedType[Byte](RawType.BYTE) with Type.Integral {
  //protected val _V = manifest[Byte]
  protected[this] val default = 0.toByte
  implicit val lifter = scala.Numeric.ByteIsIntegral
  val ZERO = defaultInst
}
object CHAR extends ValuedType[Char](RawType.CHAR) with Type.Integral {
  //protected val _V = manifest[Char]
  protected[this] val default = '\0'
  implicit val lifter = scala.Numeric.CharIsIntegral
  val NUL = defaultInst
}
object DOUBLE extends ValuedType[Double](RawType.DOUBLE) with Type.Fractional {
  //protected val _V = manifest[Double]
  protected[this] val default = 0.0
  implicit val lifter = scala.Numeric.DoubleIsFractional
}
object FLOAT extends ValuedType[Float](RawType.FLOAT) with Type.Fractional {
  //protected val _V = manifest[Float]
  protected[this] val default = 0f
  implicit val lifter = scala.Numeric.FloatIsFractional
}
object INT extends ValuedType[Int](RawType.INT) with Type.Integral {
  //protected val _V = manifest[Int]
  implicit val lifter = scala.Numeric.IntIsIntegral
  implicit def asBoolean(inst:Instance) = inst.self!=0
  protected[this] val default = 0
  val ZERO = defaultInst
  val ONE  = instance(1, Val.Bottom)
  val NEG_ONE = instance(-1, Val.Bottom)
  override protected[this] def cacheHook = {
    case 0  => ZERO
    case 1  => ONE
    case -1 => NEG_ONE
  }
}
object LONG extends ValuedType[Long](RawType.LONG) with Type.Integral {
  //protected val _V = manifest[Long]
  protected[this] val default = 0L
  implicit val lifter = scala.Numeric.LongIsIntegral
  val ZERO = defaultInst
}
object SHORT extends ValuedType[Short](RawType.SHORT) with Type.Integral {
  //protected val _V = manifest[Short]
  protected[this] val default = 0.toShort
  implicit val lifter = scala.Numeric.ShortIsIntegral
  val ZERO = defaultInst
}