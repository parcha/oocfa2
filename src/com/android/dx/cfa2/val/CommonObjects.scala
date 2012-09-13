package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import cfa2._
import env._

// Common object types

object OBJECT_ extends OBJECT(RawType.OBJECT) {
  /* OBJECT supertypes all RefTypes */
  protected override def >> (t: Type) = t.isInstanceOf[RefType]
  
  protected val constructor = new Instance(_, _)
  protected[this] final class Instance_ (params: IParams, deps:Val_)
  extends super.Instance_(params, deps) {
    protected[this] final class Ref_ (env: HeapEnv) extends super.Ref_(env)
    type Ref = Ref_
    protected[this] val ref = new Ref(_)
  }
  type Instance = Instance_
}

object CLASS extends OBJECT(RawType.CLASS) {
  protected val constructor = new Instance(_, _)
  instance_param[String]('desc)
  // TODO: make use of first-class Class reflection
  instance_param_[Class[_]]('self, null)
  protected[this] final class Instance_(params: IParams, deps:Val_)
  extends super.Instance_(params, deps) {
    lazy val desc = param[String]('desc)
    protected[this] final class Ref_(env: HeapEnv) extends super.Ref_(env)
    type Ref = Ref_
    protected[this] val ref = new Ref(_)
  }
  type Instance = Instance_
}

/******* Dynamised types *************/
import java.{lang => J}
import java.{math => M}

object STRING extends Dynamic[J.String](RawType.STRING) {
  protected[this] val default = new String
}

// FIXME: These objects have no real defaults! The premise of reflection/dynamics is broken!
/*object BIG_INT extends Dynamic[M.BigInteger](RawType.intern("Ljava/math/BigInteger;"))

object BIG_DEC extends Dynamic[M.BigDecimal](RawType.intern("Ljava/math/BigDecimal;"))

object LOCALE extends Dynamic[java.util.Locale](RawType.intern("Ljava/util/Locale;"))*/