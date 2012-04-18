package com.android.dx.cfa2.`val`

import com.android.dx
import dx.cfa2
import dx.rop.`type`.{Type => RawType, _}
import dx.rop.code.Exceptions._
import cfa2.env._

// Built-in exceptions
object Exceptionals {
  
  abstract class THROWABLE protected (raw:RawType) extends OBJECT(raw) with Type.NonFinal {
    lazy val isUnchecked = ((this < RUNTIME) | (this < ERROR))
  }
  object THROWABLE extends THROWABLE(RawType.THROWABLE) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class EXCEPTION protected (raw:RawType) extends THROWABLE(raw) with Type.NonFinal
  object EXCEPTION extends EXCEPTION(RawType.intern("Ljava/lang/Exception;")) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class RUNTIME protected (raw:RawType) extends EXCEPTION(raw) with Type.NonFinal
  object RUNTIME extends RUNTIME(RawType.intern("Ljava/lang/RuntimeException;")) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class ARITHMETIC protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object ARITHMETIC extends ARITHMETIC(TYPE_ArithmeticException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class ARRAY_INDEX protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object ARRAY_INDEX extends ARRAY_INDEX(TYPE_ArrayIndexOutOfBoundsException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class ARRAY_STORE protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object ARRAY_STORE extends ARRAY_STORE(TYPE_ArrayStoreException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class CLASS_CAST protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object CLASS_CAST extends CLASS_CAST(TYPE_ClassCastException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class ILLEGAL_MONITOR protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object ILLEGAL_MONITOR extends ILLEGAL_MONITOR(TYPE_IllegalMonitorStateException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class NEGATIVE_ARRAY_SIZE protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object NEGATIVE_ARRAY_SIZE extends NEGATIVE_ARRAY_SIZE(TYPE_NegativeArraySizeException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class NULL_POINTER protected (raw:RawType) extends RUNTIME(raw) with Type.NonFinal
  object NULL_POINTER extends NULL_POINTER(TYPE_NullPointerException) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
  abstract class ERROR protected (raw:RawType) extends THROWABLE(raw) with Type.NonFinal
  object ERROR extends ERROR(TYPE_Error) {
    protected val constructor = new Instance_(_, _)
    protected class Instance_ private[`val`] (params: IParams, deps: Val_)
    extends super.Instance_(params, deps) {
      class Ref_ protected[Instance_] (env: HeapEnv) extends super.Ref_(env)
      type Ref = Ref_
      protected[this] val ref = new Ref(_)
    }
    type Instance = Instance_
  }
  
}