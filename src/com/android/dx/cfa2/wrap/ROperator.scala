package com.android.dx.cfa2.wrap

import com.android.dx
import dx.rop.code.{Rop, RegOps}
import dx.cfa2
import cfa2._
import `val`._

/** Wrapper for Rop */
sealed case class ROperator protected (private val raw:Rop) extends Immutable with NotNull {
  import ROpCodes._
  import ROperator._
  
  if(opcode.arity != -42)
    assert(opcode.isCompatibleArity(raw.getSources.size))
  
  def opcode:OpCode = ROpCodes(raw.getOpcode).asInstanceOf[OpCode]
  def branchingness = Branchingnesses(raw.getBranchingness)
  def resultType = raw.getResult
  def sourceTypes = raw.getSources
  def exceptionTypes = raw.getExceptions
  def nickname = raw.getNickname
  def isCallLike = raw isCallLike
}
object ROperator {
  implicit def wrap(raw:Rop) = new ROperator(raw)
  implicit def unwrap(op:ROperator) = op.raw
  
  object Branchingnesses extends Enumeration(1) {
    type Branchingness = Value
    val NONE, RETURN, GOTO, IF, SWITCH, THROW = Value
  }
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
  
  sealed trait OnObject {
    // Default common case
    val objOperand : Int = 0
  }
  
  sealed trait WithConstant { type Constant <: dx.rop.cst.TypedConstant }
  
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
  
  sealed abstract class Arith protected (arity:Int = 2)
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
      arity:Int = 2)
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
      else throw new RuntimeException
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
  
  sealed class End private[ROpCodes](arity:Int) extends OpCode(arity) with Branches
  
  val RETURN = new End(-2)
  val ARRAY_LENGTH = new OpCode(1)
  val THROW = new End(1)
  
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
  val FILLED_NEW_ARRAY = new Allocate(1) with WithConstant
  
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

/** Wrapped operators */
object ROperators {
  import dx.rop.code.Rops
  
  case object NOP extends ROperator(Rops.NOP)
  case object MOVE_INT extends ROperator(Rops.MOVE_INT)
  case object MOVE_LONG extends ROperator(Rops.MOVE_LONG)
  case object MOVE_FLOAT extends ROperator(Rops.MOVE_FLOAT)
  case object MOVE_DOUBLE extends ROperator(Rops.MOVE_DOUBLE)
  case object MOVE_OBJECT extends ROperator(Rops.MOVE_OBJECT)
  case object MOVE_RETURN_ADDRESS extends ROperator(Rops.MOVE_RETURN_ADDRESS)
  case object MOVE_PARAM_INT extends ROperator(Rops.MOVE_PARAM_INT)
  case object MOVE_PARAM_LONG extends ROperator(Rops.MOVE_PARAM_LONG)
  case object MOVE_PARAM_FLOAT extends ROperator(Rops.MOVE_PARAM_FLOAT)
  case object MOVE_PARAM_DOUBLE extends ROperator(Rops.MOVE_PARAM_DOUBLE)
  case object MOVE_PARAM_OBJECT extends ROperator(Rops.MOVE_PARAM_OBJECT)
  case object CONST_INT extends ROperator(Rops.CONST_INT)
  case object CONST_LONG extends ROperator(Rops.CONST_LONG)
  case object CONST_FLOAT extends ROperator(Rops.CONST_FLOAT)
  case object CONST_DOUBLE extends ROperator(Rops.CONST_DOUBLE)
  case object CONST_OBJECT extends ROperator(Rops.CONST_OBJECT)
  case object CONST_OBJECT_NOTHROW extends ROperator(Rops.CONST_OBJECT_NOTHROW)
  case object GOTO extends ROperator(Rops.GOTO)
  case object IF_EQZ_INT extends ROperator(Rops.IF_EQZ_INT)
  case object IF_NEZ_INT extends ROperator(Rops.IF_NEZ_INT)
  case object IF_LTZ_INT extends ROperator(Rops.IF_LTZ_INT)
  case object IF_GEZ_INT extends ROperator(Rops.IF_GEZ_INT)
  case object IF_LEZ_INT extends ROperator(Rops.IF_LEZ_INT)
  case object IF_GTZ_INT extends ROperator(Rops.IF_GTZ_INT)
  case object IF_EQZ_OBJECT extends ROperator(Rops.IF_EQZ_OBJECT)
  case object IF_NEZ_OBJECT extends ROperator(Rops.IF_NEZ_OBJECT)
  case object IF_EQ_INT extends ROperator(Rops.IF_EQ_INT)
  case object IF_NE_INT extends ROperator(Rops.IF_NE_INT)
  case object IF_LT_INT extends ROperator(Rops.IF_LT_INT)
  case object IF_GE_INT extends ROperator(Rops.IF_GE_INT)
  case object IF_LE_INT extends ROperator(Rops.IF_LE_INT)
  case object IF_GT_INT extends ROperator(Rops.IF_GT_INT)
  case object IF_EQ_OBJECT extends ROperator(Rops.IF_EQ_OBJECT)
  case object IF_NE_OBJECT extends ROperator(Rops.IF_NE_OBJECT)
  case object SWITCH extends ROperator(Rops.SWITCH)
  case object ADD_INT extends ROperator(Rops.ADD_INT)
  case object ADD_LONG extends ROperator(Rops.ADD_LONG)
  case object ADD_FLOAT extends ROperator(Rops.ADD_FLOAT)
  case object ADD_DOUBLE extends ROperator(Rops.ADD_DOUBLE)
  case object SUB_INT extends ROperator(Rops.SUB_INT)
  case object SUB_LONG extends ROperator(Rops.SUB_LONG)
  case object SUB_FLOAT extends ROperator(Rops.SUB_FLOAT)
  case object SUB_DOUBLE extends ROperator(Rops.SUB_DOUBLE)
  case object MUL_INT extends ROperator(Rops.MUL_INT)
  case object MUL_LONG extends ROperator(Rops.MUL_LONG)
  case object MUL_FLOAT extends ROperator(Rops.MUL_FLOAT)
  case object MUL_DOUBLE extends ROperator(Rops.MUL_DOUBLE)
  case object DIV_INT extends ROperator(Rops.DIV_INT)
  case object DIV_LONG extends ROperator(Rops.DIV_LONG)
  case object DIV_FLOAT extends ROperator(Rops.DIV_FLOAT)
  case object DIV_DOUBLE extends ROperator(Rops.DIV_DOUBLE)
  case object REM_INT extends ROperator(Rops.REM_INT)
  case object REM_LONG extends ROperator(Rops.REM_LONG)
  case object REM_FLOAT extends ROperator(Rops.REM_FLOAT)
  case object REM_DOUBLE extends ROperator(Rops.REM_DOUBLE)
  case object NEG_INT extends ROperator(Rops.NEG_INT)
  case object NEG_LONG extends ROperator(Rops.NEG_LONG)
  case object NEG_FLOAT extends ROperator(Rops.NEG_FLOAT)
  case object NEG_DOUBLE extends ROperator(Rops.NEG_DOUBLE)
  case object AND_INT extends ROperator(Rops.AND_INT)
  case object AND_LONG extends ROperator(Rops.AND_LONG)
  case object OR_INT extends ROperator(Rops.OR_INT)
  case object OR_LONG extends ROperator(Rops.OR_LONG)
  case object XOR_INT extends ROperator(Rops.XOR_INT)
  case object XOR_LONG extends ROperator(Rops.XOR_LONG)
  case object SHL_INT extends ROperator(Rops.SHL_INT)
  case object SHL_LONG extends ROperator(Rops.SHL_LONG)
  case object SHR_INT extends ROperator(Rops.SHR_INT)
  case object SHR_LONG extends ROperator(Rops.SHR_LONG)
  case object USHR_INT extends ROperator(Rops.USHR_INT)
  case object USHR_LONG extends ROperator(Rops.USHR_LONG)
  case object NOT_INT extends ROperator(Rops.NOT_INT)
  case object NOT_LONG extends ROperator(Rops.NOT_LONG)
  case object ADD_CONST_INT extends ROperator(Rops.ADD_CONST_INT)
  case object ADD_CONST_LONG extends ROperator(Rops.ADD_CONST_LONG)
  case object ADD_CONST_FLOAT extends ROperator(Rops.ADD_CONST_FLOAT)
  case object ADD_CONST_DOUBLE extends ROperator(Rops.ADD_CONST_DOUBLE)
  case object SUB_CONST_INT extends ROperator(Rops.SUB_CONST_INT)
  case object SUB_CONST_LONG extends ROperator(Rops.SUB_CONST_LONG)
  case object SUB_CONST_FLOAT extends ROperator(Rops.SUB_CONST_FLOAT)
  case object SUB_CONST_DOUBLE extends ROperator(Rops.SUB_CONST_DOUBLE)
  case object MUL_CONST_INT extends ROperator(Rops.MUL_CONST_INT)
  case object MUL_CONST_LONG extends ROperator(Rops.MUL_CONST_LONG)
  case object MUL_CONST_FLOAT extends ROperator(Rops.MUL_CONST_FLOAT)
  case object MUL_CONST_DOUBLE extends ROperator(Rops.MUL_CONST_DOUBLE)
  case object DIV_CONST_INT extends ROperator(Rops.DIV_CONST_INT)
  case object DIV_CONST_LONG extends ROperator(Rops.DIV_CONST_LONG)
  case object DIV_CONST_FLOAT extends ROperator(Rops.DIV_CONST_FLOAT)
  case object DIV_CONST_DOUBLE extends ROperator(Rops.DIV_CONST_DOUBLE)
  case object REM_CONST_INT extends ROperator(Rops.REM_CONST_INT)
  case object REM_CONST_LONG extends ROperator(Rops.REM_CONST_LONG)
  case object REM_CONST_FLOAT extends ROperator(Rops.REM_CONST_FLOAT)
  case object REM_CONST_DOUBLE extends ROperator(Rops.REM_CONST_DOUBLE)
  case object AND_CONST_INT extends ROperator(Rops.AND_CONST_INT)
  case object AND_CONST_LONG extends ROperator(Rops.AND_CONST_LONG)
  case object OR_CONST_INT extends ROperator(Rops.OR_CONST_INT)
  case object OR_CONST_LONG extends ROperator(Rops.OR_CONST_LONG)
  case object XOR_CONST_INT extends ROperator(Rops.XOR_CONST_INT)
  case object XOR_CONST_LONG extends ROperator(Rops.XOR_CONST_LONG)
  case object SHL_CONST_INT extends ROperator(Rops.SHL_CONST_INT)
  case object SHL_CONST_LONG extends ROperator(Rops.SHL_CONST_LONG)
  case object SHR_CONST_INT extends ROperator(Rops.SHR_CONST_INT)
  case object SHR_CONST_LONG extends ROperator(Rops.SHR_CONST_LONG)
  case object USHR_CONST_INT extends ROperator(Rops.USHR_CONST_INT)
  case object USHR_CONST_LONG extends ROperator(Rops.USHR_CONST_LONG)
  case object CMPL_LONG extends ROperator(Rops.CMPL_LONG)
  case object CMPL_FLOAT extends ROperator(Rops.CMPL_FLOAT)
  case object CMPL_DOUBLE extends ROperator(Rops.CMPL_DOUBLE)
  case object CMPG_FLOAT extends ROperator(Rops.CMPG_FLOAT)
  case object CMPG_DOUBLE extends ROperator(Rops.CMPG_DOUBLE)
  case object CONV_L2I extends ROperator(Rops.CONV_L2I)
  case object CONV_F2I extends ROperator(Rops.CONV_F2I)
  case object CONV_D2I extends ROperator(Rops.CONV_D2I)
  case object CONV_I2L extends ROperator(Rops.CONV_I2L)
  case object CONV_F2L extends ROperator(Rops.CONV_F2L)
  case object CONV_D2L extends ROperator(Rops.CONV_D2L)
  case object CONV_I2F extends ROperator(Rops.CONV_I2F)
  case object CONV_L2F extends ROperator(Rops.CONV_L2F)
  case object CONV_D2F extends ROperator(Rops.CONV_D2F)
  case object CONV_I2D extends ROperator(Rops.CONV_I2D)
  case object CONV_L2D extends ROperator(Rops.CONV_L2D)
  case object CONV_F2D extends ROperator(Rops.CONV_F2D)
  case object TO_BYTE extends ROperator(Rops.TO_BYTE)
  case object TO_CHAR extends ROperator(Rops.TO_CHAR)
  case object TO_SHORT extends ROperator(Rops.TO_SHORT)
  case object RETURN_VOID extends ROperator(Rops.RETURN_VOID)
  case object RETURN_INT extends ROperator(Rops.RETURN_INT)
  case object RETURN_LONG extends ROperator(Rops.RETURN_LONG)
  case object RETURN_FLOAT extends ROperator(Rops.RETURN_FLOAT)
  case object RETURN_DOUBLE extends ROperator(Rops.RETURN_DOUBLE)
  case object RETURN_OBJECT extends ROperator(Rops.RETURN_OBJECT)
  case object ARRAY_LENGTH extends ROperator(Rops.ARRAY_LENGTH)
  case object THROW extends ROperator(Rops.THROW)
  case object MONITOR_ENTER extends ROperator(Rops.MONITOR_ENTER)
  case object MONITOR_EXIT extends ROperator(Rops.MONITOR_EXIT)
  case object AGET_INT extends ROperator(Rops.AGET_INT)
  case object AGET_LONG extends ROperator(Rops.AGET_LONG)
  case object AGET_FLOAT extends ROperator(Rops.AGET_FLOAT)
  case object AGET_DOUBLE extends ROperator(Rops.AGET_DOUBLE)
  case object AGET_OBJECT extends ROperator(Rops.AGET_OBJECT)
  case object AGET_BOOLEAN extends ROperator(Rops.AGET_BOOLEAN)
  case object AGET_BYTE extends ROperator(Rops.AGET_BYTE)
  case object AGET_CHAR extends ROperator(Rops.AGET_CHAR)
  case object AGET_SHORT extends ROperator(Rops.AGET_SHORT)
  case object APUT_INT extends ROperator(Rops.APUT_INT)
  case object APUT_LONG extends ROperator(Rops.APUT_LONG)
  case object APUT_FLOAT extends ROperator(Rops.APUT_FLOAT)
  case object APUT_DOUBLE extends ROperator(Rops.APUT_DOUBLE)
  case object APUT_OBJECT extends ROperator(Rops.APUT_OBJECT)
  case object APUT_BOOLEAN extends ROperator(Rops.APUT_BOOLEAN)
  case object APUT_BYTE extends ROperator(Rops.APUT_BYTE)
  case object APUT_CHAR extends ROperator(Rops.APUT_CHAR)
  case object APUT_SHORT extends ROperator(Rops.APUT_SHORT)
  case object NEW_INSTANCE extends ROperator(Rops.NEW_INSTANCE)
  case object NEW_ARRAY_INT extends ROperator(Rops.NEW_ARRAY_INT)
  case object NEW_ARRAY_LONG extends ROperator(Rops.NEW_ARRAY_LONG)
  case object NEW_ARRAY_FLOAT extends ROperator(Rops.NEW_ARRAY_FLOAT)
  case object NEW_ARRAY_DOUBLE extends ROperator(Rops.NEW_ARRAY_DOUBLE)
  case object NEW_ARRAY_BOOLEAN extends ROperator(Rops.NEW_ARRAY_BOOLEAN)
  case object NEW_ARRAY_BYTE extends ROperator(Rops.NEW_ARRAY_BYTE)
  case object NEW_ARRAY_CHAR extends ROperator(Rops.NEW_ARRAY_CHAR)
  case object NEW_ARRAY_SHORT extends ROperator(Rops.NEW_ARRAY_SHORT)
  case object CHECK_CAST extends ROperator(Rops.CHECK_CAST)
  case object INSTANCE_OF extends ROperator(Rops.INSTANCE_OF)
  case object GET_FIELD_INT extends ROperator(Rops.GET_FIELD_INT)
  case object GET_FIELD_LONG extends ROperator(Rops.GET_FIELD_LONG)
  case object GET_FIELD_FLOAT extends ROperator(Rops.GET_FIELD_FLOAT)
  case object GET_FIELD_DOUBLE extends ROperator(Rops.GET_FIELD_DOUBLE)
  case object GET_FIELD_OBJECT extends ROperator(Rops.GET_FIELD_OBJECT)
  case object GET_FIELD_BOOLEAN extends ROperator(Rops.GET_FIELD_BOOLEAN)
  case object GET_FIELD_BYTE extends ROperator(Rops.GET_FIELD_BYTE)
  case object GET_FIELD_CHAR extends ROperator(Rops.GET_FIELD_CHAR)
  case object GET_FIELD_SHORT extends ROperator(Rops.GET_FIELD_SHORT)
  case object GET_STATIC_INT extends ROperator(Rops.GET_STATIC_INT)
  case object GET_STATIC_LONG extends ROperator(Rops.GET_STATIC_LONG)
  case object GET_STATIC_FLOAT extends ROperator(Rops.GET_STATIC_FLOAT)
  case object GET_STATIC_DOUBLE extends ROperator(Rops.GET_STATIC_DOUBLE)
  case object GET_STATIC_OBJECT extends ROperator(Rops.GET_STATIC_OBJECT)
  case object GET_STATIC_BOOLEAN extends ROperator(Rops.GET_STATIC_BOOLEAN)
  case object GET_STATIC_BYTE extends ROperator(Rops.GET_STATIC_BYTE)
  case object GET_STATIC_CHAR extends ROperator(Rops.GET_STATIC_CHAR)
  case object GET_STATIC_SHORT extends ROperator(Rops.GET_STATIC_SHORT)
  case object PUT_FIELD_INT extends ROperator(Rops.PUT_FIELD_INT)
  case object PUT_FIELD_LONG extends ROperator(Rops.PUT_FIELD_LONG)
  case object PUT_FIELD_FLOAT extends ROperator(Rops.PUT_FIELD_FLOAT)
  case object PUT_FIELD_DOUBLE extends ROperator(Rops.PUT_FIELD_DOUBLE)
  case object PUT_FIELD_OBJECT extends ROperator(Rops.PUT_FIELD_OBJECT)
  case object PUT_FIELD_BOOLEAN extends ROperator(Rops.PUT_FIELD_BOOLEAN)
  case object PUT_FIELD_BYTE extends ROperator(Rops.PUT_FIELD_BYTE)
  case object PUT_FIELD_CHAR extends ROperator(Rops.PUT_FIELD_CHAR)
  case object PUT_FIELD_SHORT extends ROperator(Rops.PUT_FIELD_SHORT)
  case object PUT_STATIC_INT extends ROperator(Rops.PUT_STATIC_INT)
  case object PUT_STATIC_LONG extends ROperator(Rops.PUT_STATIC_LONG)
  case object PUT_STATIC_FLOAT extends ROperator(Rops.PUT_STATIC_FLOAT)
  case object PUT_STATIC_DOUBLE extends ROperator(Rops.PUT_STATIC_DOUBLE)
  case object PUT_STATIC_OBJECT extends ROperator(Rops.PUT_STATIC_OBJECT)
  case object PUT_STATIC_BOOLEAN extends ROperator(Rops.PUT_STATIC_BOOLEAN)
  case object PUT_STATIC_BYTE extends ROperator(Rops.PUT_STATIC_BYTE)
  case object PUT_STATIC_CHAR extends ROperator(Rops.PUT_STATIC_CHAR)
  case object PUT_STATIC_SHORT extends ROperator(Rops.PUT_STATIC_SHORT)
  case object MARK_LOCAL_INT extends ROperator(Rops.MARK_LOCAL_INT)
  case object MARK_LOCAL_LONG extends ROperator(Rops.MARK_LOCAL_LONG)
  case object MARK_LOCAL_FLOAT extends ROperator(Rops.MARK_LOCAL_FLOAT)
  case object MARK_LOCAL_DOUBLE extends ROperator(Rops.MARK_LOCAL_DOUBLE)
  case object MARK_LOCAL_OBJECT extends ROperator(Rops.MARK_LOCAL_OBJECT)
  case object FILL_ARRAY_DATA extends ROperator(Rops.FILL_ARRAY_DATA)
}