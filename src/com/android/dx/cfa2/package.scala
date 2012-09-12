package com.android.dx

import com.android.dx
import cfa2._
import scala.collection.{parallel=>par, _}

package object cfa2 {

  /* ============= Convenience ============ */
  import dx.rop.cst._
  import `val`._
  def liftConstant(cst:TypedConstant) = cst match {
    case c:CstInteger   => Val.Atom(INT.instance(c.getValue))
    case c:CstLong      => Val.Atom(LONG.instance(c.getValue))
    case c:CstFloat     => Val.Atom(FLOAT.instance(c.getValue))
    case c:CstDouble    => Val.Atom(DOUBLE.instance(c.getValue))
    case c:CstString    => Val.Atom(STRING.instance(c.getString))
    case c:CstType      => Val.Atom(CLASS.instance(Val.Bottom, ('desc, c.getDescriptor.getString)))
    case c:CstKnownNull => Val.Atom(NULL.singleton)
  }
  
  /* ======================== Parsing ================ */
  import scala.util.matching._
  /** Gleaned from empirical testing and VMSpec 2nd Ed section 4.3 **/
  val descriptorRegex = new Regex("(?x)^"+{
    val hex = "[0-9a-f]" // Lowercase only
    val base = "[BCDFIJSZV]"
    val id = "(?:"+                      //start with...
                "(?: \\p{Alpha}      |"+ //letter or...
                "    [\\$_] \\p{Alpha}"+ //...symbol+letter
                ")"+
                "[\\w\\$]*"+ //end with as many id-chars as we like
             ")"
    val obj = "(?:L (?<classname>"+
                      id+
                      "(?:/"+id+")*"+ //Can't end on a /
                   ")"+
                 ";"+
              ")"
    val pseudo = "(?:addr|null)"
    // Putting everything together...
    "\\[*"+ //Array dims
    "(?:"+
       "(?:"+
          "(?: N"+hex+"{4} )?"+ //Native?
          obj+"?"+ //Object-type
       ")|"+base+ //Or base-type
    ")|<"+pseudo+">" //Or a pseudo-type (which can't be arrayed)
  }+"$",
  "classname")
  
  /* ============ Exceptions =================== */
  /** This is an assertion/requirement stemming from reasonable program input/configuration **/
  final class PresumptionException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
  
  def presume(presumption: Boolean,
              msg: =>String = "presumption failed") = {
    if(!presumption)
      throw new PresumptionException(msg)
  }
  
  def presumably[E <: Exception](act: Unit=>Unit)
                (implicit E_ : Manifest[E]) = presumably[E, Unit](act)
  def presumably[E <: Exception, R](act: Unit=>R)
                (implicit E_ : Manifest[E]): R = {
    try {
      act()
    } catch {
      case e:E =>
        if(E_ >:> ClassManifest.fromClass(e.getClass))
          throw new PresumptionException("presumption failed", e)
        else throw e
    }
  }
  
  /* ============= Interfacing with Java codebase ======== */
  /*import dx.rop.code.{BasicBlock=>BB}
  implicit def toBBVisitor(f: (BB, BB)=>Unit) : BB.Visitor = {
    new BB.Visitor() {
      def visitBlock(v:BB, parent:BB) = f(v,parent)
    }
  }
  implicit def toBBVisitor(f: (BB)=>Unit) : BB.Visitor = {
    new BB.Visitor() {
      def visitBlock(v:BB, parent:BB) = f(v)
    }
  }
  def toBBParentVisitor(f: (BB)=>Unit) : BB.Visitor = {
    new BB.Visitor() {
      def visitBlock(v:BB, parent:BB) = f(parent)
    }
  }
  
  import dx.ssa.{SsaInsn=>Insn, _}
  def toMoveInsnVisitor(f: NormalSsaInsn=>Unit) : Insn.Visitor = {
    new Insn.Visitor() {
      def visitMoveInsn(i:NormalSsaInsn) = f(i)
      def visitPhiInsn(i:PhiInsn) = {}
      def visitNonMoveInsn(i:NormalSsaInsn) = {}
    }
  }
  def toNonMoveInsnVisitor(f: NormalSsaInsn=>Unit) : Insn.Visitor = {
    new Insn.Visitor() {
      def visitMoveInsn(i:NormalSsaInsn) = {}
      def visitPhiInsn(i:PhiInsn) = {}
      def visitNonMoveInsn(i:NormalSsaInsn) = f(i)
    }
  }
  implicit def toPhiInsnVisitor(f: PhiInsn=>Unit) : Insn.Visitor = {
    new Insn.Visitor() {
      def visitMoveInsn(i:NormalSsaInsn) = {}
      def visitPhiInsn(i:PhiInsn) = f(i)
      def visitNonMoveInsn(i:NormalSsaInsn) = {}
    }
  }*/
  
  type FieldSpec = dx.rop.cst.CstFieldRef
  type IFieldSpec = FieldSpec
  type SFieldSpec = FieldSpec
  
  type MethodSpec = dx.rop.cst.CstBaseMethodRef
  type IMethodSpec = MethodSpec
  type SMethodSpec = MethodSpec
  
  /* ============= Class-loading/lifting hackery =========== */
  import java.net.{URLClassLoader, URL}
  class AnalysisClassLoader(parent: ClassLoader, urls: Iterable[URL] = List())
  extends URLClassLoader(urls.toArray, parent) {
    
    def reflectClass(name: String) : Option[Class[_]] =
      try Some(loadClass(name))
      catch {
        case _:ClassNotFoundException =>
          warn("Couldn't find class for reflection: "+name)
          None
        case e:NoClassDefFoundError =>
          warn("In attempting to load "+name+" for reflection, a depedency could not be found: "+e.getMessage)
          None
      }
    
    private[cfa2] def +=(url: URL) = super.addURL(url)
    
    private[cfa2] def registerRawClass(name: String, data: Array[Byte]) =
      defineClass(name, data, 0, data.length)
    
    protected[this] def warn(msg:String) =
      // FIXME: HACK: for now, we just pull the singleton and warn that way
      CFA2Analysis.singleton.opts.log('warn)(msg)
  }
  
  import java.io.File
  val analysisClasspath : immutable.Seq[File] = immutable.Seq(
    "."
  ) map (new File(_))
  assert(analysisClasspath forall (_.exists))
  object BuiltinAnalysisClassLoader extends AnalysisClassLoader(ClassLoader.getSystemClassLoader,
                                                                (analysisClasspath map (_.toURL)))
  
  /* ============ Hooks ================== */
  import `val`.IParams
  type Hook[-Args, +Ret] = Args => Option[Ret]  
  type InstanceHook = Hook[(Instantiable, Val_, IParams),
                           IParams]
  type CloneHook = Hook[(Instantiable#Instance, Val_, IParams),
                        IParams]
  type UnknownMethodHook = Hook[(MethodSpec, Seq[Val_]),
                                Val_]
  type KnownMethodHook = Hook[(wrap.Method, Seq[Val_], CFA2Analysis.FSummary),
                              CFA2Analysis.FSummary]
  
  def HOOK[Args, Ret, H <: Hook[Args, Ret]]
          (hooks: Iterable[H], args: Args, act:Ret=>Unit) =
    for(hook <- hooks) hook(args) match {
      case None      =>
      case Some(ret) => act(ret)
    }
  
  /* ============= Util type stuff =========== */
  import scala.ref.SoftReference
  type Cached[K, V<:AnyRef, M[K, V] <: mutable.Map[K, V]] = {
    type Map = M[K,SoftReference[V]]
    type Cacher = Cache[K,V]
  }
  import scala.ref.WeakReference
  type Registered[K, V<:AnyRef, M[K, V] <: mutable.Map[K, V]] = {
    type Map = M[K,WeakReference[V]]
    type Registrar = Registry[K,V]
  }
  
  /* ============= String repr stuff ========= */
  
  /* ============== Linearization ============ */
  
  def linearize(row:GenIterable[String], twixt:String="") : String = {
    val str = new StringBuilder
    for(col <- row) {
      str append col
      str append twixt
    }
    str.delete(str.length-twixt.length, str.length)
    return str result
  }
  
  def linearizeMat(matrix:GenIterable[GenIterable[String]], twixtCols:String="", twixtRows:String="") : String = {
    val columnized =
    for(row <- matrix) yield
      linearize(row, twixtRows)
    return linearize(columnized, twixtCols)
  }
  
  trait HasLinearizedRepr {
    protected implicit def linearizedRepr : GenIterable[String]
    protected val twixtLines : String = "\n"
    override def toString : String = linearize(linearizedRepr, twixtLines)
  }
  
  /* ============== Columnization =============== */
  
  private type Row[E] = GenIterable[E]
  private type Col[E] = GenSeq[E]
  type Matrix[E] = Row[Col[E]]
  
  def normalize(matrix : Matrix[String], spacing:Int=0) : Matrix[String] = {
    val rows = matrix.size
    val cols = matrix.head.length
    
    val maxes =
    for(c <- 0 until cols) yield {
      val lens = for(row <- matrix) yield row(c).length
      lens.max
    }
    
    for(row <- matrix)
      yield for(c <- 0 until cols) yield {
        if(c < cols-1)
        	String.format("%-"+(maxes(c)+spacing)+"s", row(c))
        else row(c)
      }
  }
  
  trait HasColumnizedRepr {
    protected implicit def columnizedRepr : Matrix[String]
    protected val colSpacing : Int = 2 
    protected val twixtRows : String = "\n"
    override def toString : String = linearizeMat(normalize(columnizedRepr, colSpacing), twixtRows=twixtRows)
  }
  
  /* ============= Logging =============== */
  trait Dumpable {
    import java.io.PrintStream
    def dump(ps: PrintStream) = ps println this.toString
  }
  
  /* ============== Misc =================== */
  
  trait PrettyMap[K,V] extends GenMap[K, V] with HasColumnizedRepr {
    protected final def columnizedRepr : Matrix[String] =
      for((k,v) <- this) yield linearRepr(k,v)
    protected def linearRepr(k:K, v:V) = immutable.IndexedSeq(keyStr(k), valueStr(v))
    protected def keyStr(k:K) : String = k.toString
    protected def valueStr(v:V) : String = v.toString
  }
}