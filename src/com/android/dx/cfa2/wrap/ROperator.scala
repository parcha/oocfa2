package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.code.{Rop, RegOps}
import dx.cfa2
import cfa2._
import `val`._

/** Wrapper for Rop */
final case class ROperator(val raw:Rop) extends AnyVal with NotNull {
  import ROpCodes._
  import ROperator._
  
  def arity = raw.getSources.size
  def opcode:OpCode = ROpCodes(raw.getOpcode).asInstanceOf[OpCode]
  def branchingness = Branchingnesses(raw.getBranchingness)
  def resultType = raw.getResult
  def sourceTypes = raw.getSources
  def exceptionTypes = raw.getExceptions
  def nickname = raw.getNickname
  def isCallLike = raw isCallLike
}
object ROperator {
  object Branchingnesses extends Enumeration(1) {
    val NONE, RETURN, GOTO, IF, SWITCH, THROW = Value
  }
  type Branchingness = Branchingnesses.Value
}

/** Wrapped opcodes */
object ROpCodes extends Enumeration(1) {
  assume {
    //All ROpCodes correspond correctly to their numerical raw ROps
    values forall ((c:Value) => {
      val code = c.asInstanceOf[OpCode]
      code.toString.toLowerCase.replace('_', '-') == RegOps.opName(code.id)
    })
  }
  
  import scala.collection._
  
  implicit def convertFromRopCode(code:Int) = ROpCodes(code)
  
  /**
   * FIXME: HACK
   * Special arities:
   * -1 => variable
   * -2 => 0 or 1
   * -3 => 1 or 2
   * -42 => confusingly implemented
   */
  sealed class OpCode private[ROpCodes] (private[wrap] val arity:Int) extends Val with Immutable {
    final def isCompatibleArity(a: Int) = (minArity to maxArity) contains a
    final val minArity =
      if(arity >= 0) arity
      else if(arity == -1) 0
      else arity.abs-2
    final val maxArity =
      if(arity >= 0) arity
      else if(arity == -1) Int.MaxValue-1
      else arity.abs-1
    final def hasMixedArity = arity < -1
    final def isVarArity = arity == -1
  }
  
  //sealed trait Assigns extends OpCode
  sealed trait NoResult extends OpCode
  sealed trait Branches extends OpCode with NoResult
  
  sealed trait Evaluable extends OpCode /*with Assigns*/ {
    import `val`._
    type ArgT <: Instantiable
    type RetT <: Instantiable
    final type Arg = VAL[ArgT]
    final type Ret = VAL[RetT]
    def eval(retT: RetT, args:`val`.Val[ArgT]*) : `val`.Val[RetT]
  }
  /** Result may depend on the type of the result */
  sealed trait PolymorphicallyEvaluable extends Evaluable {
    import `val`._
    // Just a default in case we actually handle the unknown case
    def eval(retT: RetT, args:`val`.Val[ArgT]*) : `val`.Val[RetT] = {
      require(args.length == maxArity)
      Val.eval (eval_(retT, _:Seq[Arg])) (args:_*)
    }
    @inline
    protected[this] def eval_(retT: RetT, args:Seq[Arg]) : Ret
  }
  /** Evaluation doesn't depend on the type of the result */
  sealed trait FixedEvaluable extends PolymorphicallyEvaluable {
    protected[this] final def eval_(retT: RetT, args: Seq[Arg]) = eval_(args)
    @inline
    protected[this] def eval_(args:Seq[Arg]): Ret
  }
  
  sealed trait Reflected extends Evaluable {
    type EigenArg
    type EigenRet
    type ArgT <: `val`.Reflected[EigenArg]
    type RetT <: `val`.Reflected[_<:EigenRet]
    def func(retT:RetT, args:Option[EigenArg]*) : Option[EigenRet]
  }
  sealed trait PolymorphicallyReflected extends Reflected with PolymorphicallyEvaluable {
    protected[this] def eval_(retT: RetT, args: Seq[Arg]): Ret = {
      val args_ = for(arg <- args) yield
        if(arg.isUnknown) None
        else Some(arg.asInstanceOf[ArgT#Instance].self.asInstanceOf[EigenArg])
      func(retT, args_ :_*) match {
        case Some(ret) => retT.instance(ret.asInstanceOf[retT.EigenType], Val(args:_*))
        //`val`.Reflected.reflect(ret).asInstanceOf[Ret]
        case None      => retT.unknown
      }
    }
    // Just a default in case we can actually handle the unknown case
    @inline
    def func(retT:RetT, args:Option[EigenArg]*) : Option[EigenRet] =
      if(args contains None) None
      else Some(func_(retT, (args map {_.get})))
    @inline
    protected[this] def func_[A <: EigenArg](retT:RetT, args:Seq[A]): EigenRet
  }
  sealed trait FixedReflected extends PolymorphicallyReflected {
    protected[this] final override def func_[A <: EigenArg](retT: RetT, args: Seq[A]) = func_(args)
    @inline
    protected[this] def func_[A <: EigenArg](args:Seq[A]): EigenRet
  }
  
  sealed trait OnObject extends OpCode {
    // Default common case
    val objOperand : Int = 0
  }
  
  sealed trait WithConstant extends OpCode { type Constant <: dx.rop.cst.TypedConstant }
  
  val NOP = new OpCode(0) with NoResult
  
  sealed class Move private[ROpCodes](arity:Int) extends OpCode(arity) /*with Assigns*/
  val MOVE = new Move(1)
  val MOVE_PARAM = new Move(0)
  val MOVE_EXCEPTION = new Move(0)
  
  val CONST = new OpCode(0) with WithConstant
  val GOTO = new OpCode(0) with Branches
  
  sealed class If private[ROpCodes] extends OpCode(-3) with Branches {
    // Must be lazy so that we can register the flipped version first
    lazy val flipped = ROpCodes(RegOps.flippedIfOpcode(id)).asInstanceOf[If]
  }
  sealed class IfEq private[ROpCodes] extends If
  val IF_EQ = new IfEq
  val IF_NE = new IfEq
  sealed class IfCmp private[ROpCodes] extends If
  val IF_LT = new IfCmp
  val IF_GE = new IfCmp
  val IF_LE = new IfCmp
  val IF_GT = new IfCmp
  
  val SWITCH = new OpCode(1) with Branches
  
  sealed abstract class Arith protected (arity:Int = -3)
  extends OpCode(arity) with FixedReflected {
    type EigenArg = AnyVal
    type EigenRet = AnyVal //Int or Long
    type ArgT <: Type.Numeric with `val`.Reflected[EigenArg]
    final type RetT = ArgT
  }
  
  //DALVIK: Dalvik only has int and long as operator-usable integrals
  sealed class GeneralArith private[ROpCodes] (
      intF:   (Int*)   =>Int,
      longF:  (Long*)  =>Long,
      floatF: (Float*) =>Float,
      doubleF:(Double*)=>Double,
      arity:Int = -3)
  extends Arith(arity) {
    protected[this] final def func_[@specialized(Int, Long, Float, Double) A <: EigenArg]
                                   (args: Seq[A]) = args(0) match {
      case _:Int    => intF   (args.asInstanceOf[Seq[Int]]:_*)
      case _:Long   => longF  (args.asInstanceOf[Seq[Long]]:_*)
      case _:Float  => floatF (args.asInstanceOf[Seq[Float]]:_*)
      case _:Double => doubleF(args.asInstanceOf[Seq[Double]]:_*)
    }
  }
  val ADD = new GeneralArith(
  (a)=>a(0)+a(1), (a)=>a(0)+a(1), (a)=>a(0)+a(1), (a)=>a(0)+a(1))
  val SUB = new GeneralArith(
  (a)=>a(0)-a(1), (a)=>a(0)-a(1), (a)=>a(0)-a(1), (a)=>a(0)-a(1))  
  val MUL = new GeneralArith(
  (a)=>a(0)*a(1), (a)=>a(0)*a(1), (a)=>a(0)*a(1), (a)=>a(0)*a(1))
  val DIV = new GeneralArith(
  (a)=>a(0)/a(1), (a)=>a(0)/a(1), (a)=>a(0)/a(1), (a)=>a(0)/a(1))
  val REM = new GeneralArith(
  (a)=>a(0)%a(1), (a)=>a(0)%a(1), (a)=>a(0)%a(1), (a)=>a(0)%a(1))
  val NEG = new GeneralArith(
  (a)=>(-a(0)), (a)=>(-a(0)), (a)=>(-a(0)), (a)=>(-a(0)),
  1)
  
  sealed class IntegralArith private[ROpCodes] (
      intF:   (Int*)  =>Int,
      longF:  (Long*) =>Long,
      arity:Int = -3)
  extends Arith(arity) {
    type ArgT <: Type.Integral with `val`.Reflected[EigenArg]
    protected[this] final def func_[@specialized(Int, Long) A <: EigenArg]
                                   (args: Seq[A]) = args(0) match {
      case _:Int    => intF (args.asInstanceOf[Seq[Int]]:_*)
      case _:Long   => longF(args.asInstanceOf[Seq[Long]]:_*)
    }
  }
  val AND = new IntegralArith(
  (a)=>a(0)&a(1), (a)=>a(0)&a(1))
  val OR = new IntegralArith(
  (a)=>a(0)|a(1), (a)=>a(0)|a(1))
  val XOR = new IntegralArith(
  (a)=>a(0)^a(1), (a)=>a(0)^a(1))
  //FIXME: JVM types differ from Dalvik types
  //NOTE: a(1) is actually always an Int
  val SHL = new IntegralArith(
  (a)=>a(0)<<a(1), (a)=>a(0)<<a(1))
  //FIXME: JVM types differ from Dalvik types
  //NOTE: a(1) is actually always an Int
  val SHR = new IntegralArith(
  (a)=>a(0)>>a(1), (a)=>a(0)>>a(1))
  //FIXME: JVM types differ from Dalvik types
  //NOTE: a(1) is actually always an Int
  val USHR = new IntegralArith(
  (a)=>a(0)>>>a(1), (a)=>a(0)>>>a(1))
  val NOT = new IntegralArith(
  (a)=>(~a(0)), (a)=>(~a(0)),
  1)
  
  sealed abstract class Comparison extends OpCode(2) with FixedReflected {
    type EigenArg = AnyVal
    type EigenRet = Int
  }
  object Comparison {
    sealed class CMP
    case object LT extends CMP
    case object EQ extends CMP
    case object GT extends CMP
    implicit def lower(cmp:CMP) : Int = cmp match {
      case LT => -1
      case EQ => 0
      case GT => 1
    }
    def lift(i:Int) : CMP =
      if(i < 0)       LT
      else if(i == 0) EQ
      else if(i > 0)  GT
      else throw new InternalError // Impossible
  }
  val CMPL = new Comparison {
    import Comparison._
    //DALVIK: Dalvik only uses long for integrals here
    //TODO: Ensure the semantics regarding NaN are being followed
    protected[this] final def func_[@specialized(Long, Float, Double) A <: EigenArg]
                                   (args: Seq[A]) = args(0) match {
      case _:Long => {
        val a = args.asInstanceOf[Seq[Long]]
        (a(0) compare a(1)).signum
      }
      case _:Float => {
        val a = args.asInstanceOf[Seq[Float]]
        (a(0) compare a(1)).signum
      }
      case _:Double => {
        val a = args.asInstanceOf[Seq[Double]]
        (a(0) compare a(1)).signum
      }
    }
  }
  val CMPG = new Comparison {
    import Comparison._
    //TODO: Implement the semantics regarding NaN
    protected[this] final def func_[@specialized(Float, Double) A <: EigenArg]
                                   (args: Seq[A]) = args(0) match {
      case _:Float => {
        val a = args.asInstanceOf[Seq[Float]]
        (a(0) compare a(1)).signum
      }
      case _:Double => {
        val a = args.asInstanceOf[Seq[Double]]
        (a(0) compare a(1)).signum
      }
    }
  }
  
  sealed abstract class Conversion private[ROpCodes] extends OpCode(1) with PolymorphicallyReflected
  val CONV = new Conversion {
    // Because Scala doesn't have a supertype for the numeric primitives
    type NumVal = AnyVal {
      def toInt:    Int
      def toLong:   Long
      def toFloat:  Float
      def toDouble: Double
    }
    type EigenArg = NumVal
    type EigenRet = NumVal
    type RetT = `val`.Reflected[_<:EigenRet] with Type.Numeric
    protected[this] final def func_[@specialized(Int, Long, Float, Double) A <: EigenArg]
                                   (retT: RetT, args: Seq[A]) = retT match {
      case `val`.INT    => args(0).toInt
      case `val`.LONG   => args(0).toLong
      case `val`.FLOAT  => args(0).toFloat
      case `val`.DOUBLE => args(0).toDouble
    }
  }
  
  sealed class IntegralConversion private[ROpCodes] (f:Int=>Int)
  extends Conversion with FixedReflected {
    type EigenArg = Int
    type EigenRet = Int
    protected[this] final def func_[@specialized(Int) A <: EigenArg](args: Seq[A]) = f(args(0))
  }
  val TO_BYTE = new IntegralConversion((i)=> (i << 24) >> 24)
  val TO_CHAR = new IntegralConversion((i)=> i & 0xFFFF)
  val TO_SHORT = new IntegralConversion((i)=> (i << 16) >> 16)
  
  sealed trait MayEnd extends OpCode
  sealed trait End extends OpCode with MayEnd
  
  val RETURN = new OpCode(-2) with End
  val ARRAY_LENGTH = new OpCode(1)
  val THROW = new OpCode(1) with MayEnd
  
  sealed class Monitor private[ROpCodes] extends OpCode(1) with NoResult with OnObject
  val MONITOR_ENTER = new Monitor
  val MONITOR_EXIT = new Monitor
  
  sealed class ArrayOp private[ROpCodes] (arity:Int) extends OpCode(arity)
  val AGET = new ArrayOp(2)
  val APUT = new ArrayOp(3) with NoResult
  
  sealed class Allocate private[ROpCodes] (arity:Int) extends OpCode(arity)
  val NEW_INSTANCE = new Allocate(0) with WithConstant
  // TODO: How does this operation actually behave?!? Sometimes it takes 1, sometimes a constant...
  val NEW_ARRAY = new Allocate(-42) with WithConstant
  val FILLED_NEW_ARRAY = new Allocate(-42) with WithConstant
  
  sealed class Caster private[ROpCodes] extends OpCode(1) with OnObject with WithConstant
  val CHECK_CAST = new Caster
  val INSTANCE_OF = new Caster
  
  sealed class Getter private[ROpCodes] (arity:Int) extends OpCode(arity)
  with WithConstant {
    type Constant = FieldSpec
  }
  val GET_FIELD = new Getter(1) with OnObject
  val GET_STATIC = new Getter(0)
  
  sealed class Putter private[ROpCodes] (arity:Int) extends OpCode(arity)
  with NoResult with WithConstant {
    type Constant = FieldSpec
  }
  val PUT_FIELD = new Putter(2) with OnObject { override val objOperand = 1 }
  val PUT_STATIC = new Putter(1)
  
  // They DO NOT branch
  sealed class Call private[ROpCodes] extends OpCode(-1) with NoResult with WithConstant {
    type Constant = MethodSpec
  }
  val INVOKE_STATIC = new Call
  val INVOKE_VIRTUAL = new Call with OnObject
  val INVOKE_SUPER = new Call with OnObject
  val INVOKE_DIRECT = new Call with OnObject
  val INVOKE_INTERFACE = new Call with OnObject
  
  val MARK_LOCAL = new OpCode(1) with NoResult
  
  // Continue Moves
  val MOVE_RESULT = new Move(0)
  val MOVE_RESULT_PSEUDO = new Move(0)
  
  val FILL_ARRAY_DATA = new OpCode(-1)
}