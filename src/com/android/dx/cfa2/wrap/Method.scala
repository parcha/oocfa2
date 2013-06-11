package com.android.dx.cfa2.wrap

import com.android.dx
import dx.cf.iface.{Method => Raw}
import dx.rop.code.RopMethod
import dx.cfa2
import cfa2._
import `val`._
import prop._
import Properties._
import parsers._
import adt.Cacher
import scala.collection._
import java.lang.{Class => JClass}
import java.lang.reflect.{Member => JMember, Method => JMethod, Constructor => JConstructor}

sealed abstract class _MethodDesc[+Self <: _MethodDesc[Self]] extends Immutable with NotNull { _:Self =>
  val name: String
  val parent: ClassDesc
  final lazy val id = {
    val pname = parent.typ.toHuman
    IDParsers.parse(IDParsers.meth_id, pname+"."+name) match {
      case IDParsers.Success(id, _) => id
      case _ => throw new InternalError(s"Failure in parsing the method descriptor for $pname.$name")
    }
  }
  
  final def arity: Int = argTs.size
  def argTs: Seq[Instantiable]
  def retT: Instantiable
  final lazy val exnTs: Option[Seq[Exceptional]] = exnCs match {
    case None     => None
    case Some(cs) =>
      Some(for(c <- cs) yield Type(c).asInstanceOf[Exceptional]) 
  }
  
  def argCs : Option[Seq[JClass[_]]]
  def exnCs : Option[Seq[JClass[_]]]
  
  type Reflected = Self with _ReflMethodDesc[Self]
  def ifReflected[R](_then: =>Reflected=>R)(_else: =>R): R = this match {
    case self:Reflected => _then(self)
    case _               => _else
  }
  val reflected = ifReflected[Option[Reflected]] {Some(_)} {None}
  
  type Dalviked = Self with _DalvikMethodDesc[Self]
  def ifDalviked[R](_then: =>Dalviked=>R)(_else: =>R): R = this match {
    case self:Dalviked => _then(self)
    case _              => _else
  }
  val dalviked = ifDalviked[Option[Dalviked]] {Some(_)} {None}
  
  /** Get the overload in klass that matches this method, if any **/
  final def matchingOverload(klass: JClass[_]): Option[JMethod] = argCs match {
    case None  => None
    case Some(argCs) =>
      import analysis.CFA2Analysis.{log, logs} // For debugging
      var curr = klass
      // Recurse through the type hierarchy, searching for the method
      while(curr != null) {
        try {
          //CFA2Analysis.singleton.opts.log('debug) ("Seeking reflection for "+name)
          // DeclaredMethod vs. Method so that we can access private/protected
          val refl = curr.getDeclaredMethod(name, argCs:_*)
          //CFA2Analysis.singleton.opts.log('debug) ("Found as "+refl)
          assert(refl != null)
          return Some(refl)
        }
        catch {
          case e:NoSuchMethodException =>
            curr = curr.getSuperclass()
          case e:Exception =>
            log('debug) (s"Failed to find matching overload for $name in $klass along path $curr")
            e.printStackTrace(logs('error).stream)
            return None
          // Hack until we can get the classloader to cooperate
          case e:NoClassDefFoundError =>
            //log('error) (s"Classloader being uncooperative in reflection for $name in $klass along path $curr")
            //e.printStackTrace(logs('error).stream)
            return None
        }
      }
      log('debug) (s"Failed to find matching overload for $name in $klass")
      return None
  }
  
  final val isFinal = is(Final) | ((parent is Final) == Tri.T)
  final val isIMethod = !is(Static)
  
  final def is(prop: Property) = {
    assert(Domain.Method contains prop)
    _is(prop)
  }
  protected[this] def _is(prop: Property): Tri
  
  final val isAccessible = parent.isAccessible & {
    (this is Properties.Public) |
    ((this is Properties.Protected) & !(this is Final))
  }
  
  // FIXME: Need to figure out how to /always/ determine whether it's an instance method or not
  final lazy val isLiftableI = {
    assert(+isIMethod)
    val typ = parent.typ.asInstanceOf[OBJECT]
    typ.isInstanceOf[Dynamic[_]] &&
    retT.isInstanceOf[`val`.Reflected[_]] &&
    argCs != None
  }
  final lazy val isLiftableS = {
    assert(-isIMethod)
    ifReflected { self =>
      Dynamic.smeth_whitelist exists {self.refl == _}
    } {false}
  }
  
  override def toString = id+"/"+arity
}
object _MethodDesc {
  private val strawmanComparator = new java.util.Comparator[MethodDesc] {
    def compare(m1, m2) = m1.id.toString.compareTo(m2.id.toString) match {
      case 0 => ClassDesc.strawmanComparator.compare(m1.parent, m2.parent)
      case i => i
    }
  }
  implicit def strawmanOrdering[T <: MethodDesc] = mkCovariantOrdering[MethodDesc, T](strawmanComparator)
}

sealed trait _DalvikMethodDesc[+Self <: _DalvikMethodDesc[Self]] extends _MethodDesc[Self] { _:Self =>
  final def name = nat.getName.getString
  
  def prototype: dx.rop.`type`.Prototype
  final lazy val argTs =
    for(i <- 0 until prototype.getParameterTypes.size)
      // TODO: Should this be ParameterFrameTypes?
      yield Type(prototype.getParameterTypes.get(i)).asInstanceOf[Instantiable]
  final lazy val retT = Type(prototype.getReturnType).asInstanceOf[Instantiable]
  
  // Reflection support
  final lazy val argCs =
    if(_argCs contains None) None
    else Some(_argCs.flatten)
  /** More primitive argCs that lets one see which args in particular failed **/
  final lazy val _argCs : Seq[Option[JClass[_]]] =
    for(i <- 0 until arity;
        val t = prototype.getParameterTypes.get(i))
      yield
        Type(t).asInstanceOf[Instantiable].klass match {
          case null  => None
          case klass => Some(klass)
        }
  val exnCs = None
  
  final override type Dalviked = Self
  final override def ifDalviked[R](_then: =>Dalviked=>R)(_else: =>R) = _then(this)
  final override val dalviked = Some(this)
  
  protected[this] def nat: dx.rop.cst.CstNat
  final val isInstanceInit = nat.isInstanceInit
  final val isClassInit = nat.isClassInit
}
object _DalvikMethodDesc {
  // HACK: no recursive type aliases in Scala :/
  private[wrap] type Reflected[M <: _DalvikMethodDesc[M]] = M with _ReflMethod[_]
  @inline
  private[wrap] def getReflection[M <: _DalvikMethodDesc[M]](m:M)
                                 (constrM: =>JMethod=>Reflected[M])
                                 //(constrC: =>JConstructor[_]=>Reflected[M])
                                 : M = m.parent.typ match {
    // Arrays can have methods, too
    case typ:RefType => typ.klass match {
      case null  => m
      case klass => m.matchingOverload(klass) match {
        case None        => m
        case Some(_refl) => constrM(_refl)
      }
    }
  }
}

sealed case class DalvikMethod private (val raw:Raw, val rop:RopMethod)
extends _DalvikMethodDesc[DalvikMethod] {
  lazy val blocks: BasicBlockSet = {
    val bbs = rop.getBlocks
    new BasicBlockSet(
      (for(i <- 0 until bbs.size) yield BasicBlock.wrap(bbs.get(i), this)).toSet)
  }
  def firstBlock = blocks(rop.getFirstLabel)
  
  private[this] lazy val props = prop.Range(prop.Domain.Method, accessFlags)
  protected[this] def _is(prop: Property) = props contains prop
  val parent = GhostClass.wrap(raw.getDefiningClass)
  def accessFlags = raw.getAccessFlags
  // FIXME: We need to exclude the "this" so we can vary on it; also lines up with Ghost and Refl
  def prototype = raw.getEffectiveDescriptor
  def attributes(attr: String) = raw.getAttributes.findFirst(attr)
  
  protected[this] def nat = raw.getNat
  lazy val dump = rop.dump
}
object DalvikMethod extends Cacher[(Raw, RopMethod), DalvikMethod] {
  private def intern(m:DalvikMethod): DalvikMethod = cache.cache((m.raw, m.rop), m)
  def wrap(raw:Raw, rop:RopMethod) = cache cachedOrElse ((raw, rop), {
    // Upgrade to ReflMethod if we can
    val m = new DalvikMethod(raw, rop)
    val m_ = _DalvikMethodDesc.getReflection(m) { _refl =>
      type Refl = _DalvikMethodDesc.Reflected[DalvikMethod]
      new DalvikMethod(raw, rop) with _ReflMethod[Refl] {
        val refl = _refl
      }
    }
    intern(m_)
  })
  implicit def unwrap(m:DalvikMethod) = m.raw
}

sealed case class GhostMethod private (val spec:MethodSpec)
extends _DalvikMethodDesc[GhostMethod] {
  def prototype = spec.getPrototype
  val parent = GhostClass.wrap(spec.getDefiningClass)
  protected[this] def nat = spec.getNat
  protected[this] def _is(prop: Property) = Tri.U
}
object GhostMethod extends Cacher[MethodSpec, GhostMethod] {
  private def intern(m:GhostMethod) = cache.cache (m.spec, m)
  implicit def wrap(spec:MethodSpec) = cache cachedOrElse (spec, {
    // Upgrade to ReflMethod if we can
    val m = new GhostMethod(spec)
    val m_ = _DalvikMethodDesc.getReflection(m) { _refl =>
      type Refl = _DalvikMethodDesc.Reflected[GhostMethod]
      new GhostMethod(spec) with _ReflMethod[Refl] {
        val refl = _refl
      }
    }
    intern(m_)
  })
  implicit def unwrap(gm:GhostMethod) = gm.spec
}


sealed trait _ReflMethodDesc[+Self <: _ReflMethodDesc[Self]] extends _MethodDesc[Self] { _:Self =>
  val refl: JMethod // TODO: Change back to JMember when we support Constructors
  final def name = refl.getName
  final lazy val parent = ReflClass(refl.getDeclaringClass())
  
  final def argTs = for(klass <- argCs.get) yield Type(klass).asInstanceOf[Instantiable]
  
  final override type Reflected = Self
  final override def ifReflected[R](_then: =>Reflected=>R)(_else: =>R) = _then(this)
  final override val reflected = Some(this)
  
  private[this] final lazy val props = Domain.Method.fromJMember(refl)
  override protected[this] final def _is(prop) = props(prop)
  
  override final def toString = refl toString
}

sealed trait _ReflMethod[+Self <: _ReflMethod[Self]]
extends _ReflMethodDesc[Self] { _:Self =>
  val refl: JMethod
  final def retT = Type(refl.getReturnType).asInstanceOf[Instantiable]
  final def argCs = Some(refl.getParameterTypes.toSeq)
  final def exnCs = Some(refl.getExceptionTypes)
}
final case class ReflMethod private (val refl: JMethod) extends _ReflMethod[ReflMethod]
object ReflMethod extends Cacher[JMethod, ReflMethod] {
  private def intern(m:ReflMethod) = cache.cache (m.refl, m)
  implicit def wrap(refl:JMethod) = cache cachedOrElse (refl, intern(new ReflMethod(refl)))
  implicit def unwrap(gm:ReflMethod) = gm.refl
}

/*sealed trait _ReflConstructor[+Self <: _ReflConstructor[Self]]
extends _ReflMethodDesc[Self] { _:Self =>
  val refl: JConstructor[_]
  final def retT = Type(refl.getDeclaringClass).asInstanceOf[Instantiable]
  final def argCs = Some(refl.getParameterTypes)
  final def exnCs = Some(refl.getExceptionTypes)
}

final case class ReflConstructor protected (val refl: JConstructor[_])
extends _ReflConstructor[ReflConstructor]*/