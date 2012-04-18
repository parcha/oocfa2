package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import env._

sealed trait BoxedType[+ContainedType <: PrimitiveType] extends OBJECT {
  final type Contents = ContainedType
  val containedType: ContainedType
  
  protected final override def >> (t: Type) = false
}
object BoxedType {
  object VOID extends OBJECT(RawType.VOID_CLASS) with BoxedType[`val`.VOID.type] with Type.Singular {
    val containedType = `val`.VOID
    protected[this] val constructor = (params:IParams, deps:Val_)=>Instance_ 
    protected[this] object Instance_ extends super.Instance_(paramify(), Val.Bottom) {
      protected[this] final class Ref_(env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  sealed abstract class BoxedValuedType[V <: AnyRef, +T <: ValuedType[_]] protected
  (raw:RawType, val containedType: T)(implicit V_ : ClassManifest[V])
  extends Dynamic[V](raw)(V_) with BoxedType[T]
  
  import java.{lang => J}
  object BOOLEAN extends BoxedValuedType[J.Boolean, `val`.BOOLEAN.type](RawType.BOOLEAN_CLASS, `val`.BOOLEAN)
  object BYTE extends BoxedValuedType[J.Byte, `val`.BYTE.type](RawType.BYTE_CLASS, `val`.BYTE) with Type.Integral
  object CHAR extends BoxedValuedType[J.Character, `val`.CHAR.type](RawType.CHARACTER_CLASS, `val`.CHAR) with Type.Integral
  object DOUBLE extends BoxedValuedType[J.Double, `val`.DOUBLE.type](RawType.DOUBLE_CLASS, `val`.DOUBLE) with Type.Fractional
  object FLOAT extends BoxedValuedType[J.Float, `val`.FLOAT.type](RawType.FLOAT_CLASS, `val`.FLOAT) with Type.Fractional
  object INT extends BoxedValuedType[J.Integer, `val`.INT.type](RawType.INTEGER_CLASS, `val`.INT) with Type.Integral
  object LONG extends BoxedValuedType[J.Long, `val`.LONG.type](RawType.LONG_CLASS, `val`.LONG) with Type.Integral
  object SHORT extends BoxedValuedType[J.Short, `val`.SHORT.type](RawType.SHORT_CLASS, `val`.SHORT) with Type.Integral
}