package com.android.dx.cfa2.wrap

import com.android.dx
import dx.cf.iface.{Method => Raw}
import dx.rop.code.RopMethod
import dx.cfa2
import cfa2._
import `val`._
import prop.Properties._
import parsers._
import scala.collection._
import java.lang.{Class => JClass}
import java.lang.reflect.{Method => JMethod}

sealed trait MethodDesc extends Immutable with NotNull {
  def name: String
  def id: ID
  val parent: ClassDesc
  
  final def arity: Int = argTs.size
  def argTs: Seq[Instantiable]
  def retT: Instantiable
  
  def argCs : Option[Seq[JClass[_]]]
  
  def reflection: Option[JMethod]
  /** Get the overload in klass that matches this method, if any **/
  final def matchingOverload(klass: JClass[_]): Option[JMethod] = argCs match {
    case None  => None
    case Some(argCs) =>
      val opts = CFA2Analysis.singleton.opts // For debugging
      var curr = klass
      // Recurse through the type hierarchy, searching for the method
      while(curr != null) {
        try {
          //CFA2Analysis.singleton.opts.log('debug) ("Seeking reflection for "+name)
          // DeclaredMethod vs. Method so that we can access private/protected
          val refl = curr.getDeclaredMethod(name, argCs:_*)
          //CFA2Analysis.singleton.opts.log('debug) ("Found as "+refl)
          return Some(refl)
        }
        catch {
          case e:NoSuchMethodException =>
            curr = curr.getSuperclass()
          case e:Exception =>
            opts.log('debug) (s"Failed to find matching overload for $name in $klass along path $curr")
            e.printStackTrace(opts.logs('error).stream)
            return None
          // Hack until we can get the classloader to cooperate
          case e:NoClassDefFoundError =>
            opts.log('debug) (s"Classloader being uncooperative in reflection for $name in $klass along path $curr")
            e.printStackTrace(opts.logs('error).stream)
            return None
        }
      }
      CFA2Analysis.singleton.opts.log('debug) (s"Failed to find matching overload for $name in $klass")
      return None
  }
  
  val isFinal = reflection match {
    case Some(refl) => Final.testJMember(refl)
    case None       => Tri.U | (parent.isFinal == Tri.T)
  }
  val isIMethod: Tri = reflection match {
    case Some(refl) => !Static.testJMember(refl)
    case None       => Tri.U
  }
  
  // FIXME: Need to figure out how to /always/ determine whether it's an instance method or not
  final lazy val isLiftableI = {
    assert(+isIMethod)
    val typ = parent.typ.asInstanceOf[OBJECT]
    typ.isInstanceOf[Dynamic[_]] &&
    retT.isInstanceOf[Reflected[_]] &&
    argCs != None
  }
  final lazy val isLiftableS = {
    assert(-isIMethod)
    reflection match {
      case None       => false
      case Some(refl) => Dynamic.smeth_whitelist exists {refl == _}
    }
  }
  
  override final lazy val toString = reflection match {
    case Some(refl) => refl toString
    case None       => id+"/"+arity
  }
}
object MethodDesc {
  private val strawmanComparator = new java.util.Comparator[MethodDesc] {
    def compare(m1, m2) = m1.id.toString.compareTo(m2.id.toString) match {
      case 0 => ClassDesc.strawmanComparator.compare(m1.parent, m2.parent)
      case i => i
    }
  }
  implicit def strawmanOrdering[T <: MethodDesc] = mkCovariantOrdering[MethodDesc, T](strawmanComparator)
}

sealed trait DalvikMethodDesc extends MethodDesc {
  final def name = nat.getName.getString
  final lazy val id = {
    val pname = parent.typ.toHuman
    IDParsers.parse(IDParsers.meth_id, pname+"."+name) match {
      case IDParsers.Success(id, _) => id
      case _ => throw new RuntimeException("Failure in parsing the method descriptor for "+pname+"."+name)
    }
  }
  
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
  final lazy val _argCs : immutable.Seq[Option[JClass[_]]] =
    for(i <- 0 until arity;
        val t = prototype.getParameterTypes.get(i))
      yield
        Type(t).asInstanceOf[Instantiable].klass match {
          case null  => None
          case klass => Some(klass)
        }
  final lazy val reflection = parent.typ match {
    case typ:OBJECT => typ.klass match {
      case null => None
      case klass => matchingOverload(klass)
    }
  }
  
  def nat: dx.rop.cst.CstNat
  final lazy val isInstanceInit = nat.isInstanceInit
  final lazy val isClassInit = nat.isClassInit
}

final case class Method private (val raw:Raw, val rop:RopMethod) extends DalvikMethodDesc {
  lazy val blocks: BasicBlockSet = {
    val bbs = rop.getBlocks
    new BasicBlockSet(
      (for(i <- 0 until bbs.size) yield BasicBlock.wrap(bbs.get(i), this)).toSet)
  }
  def firstBlock = blocks(rop.getFirstLabel)
  
  lazy val props = prop.Range(prop.Domain.Method, accessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  val parent = GhostClass(raw.getDefiningClass)
  def accessFlags = raw.getAccessFlags
  // FIXME: We need to exclude the "this" so we can vary on it; also lines up with Ghost and Refl
  def prototype = raw.getEffectiveDescriptor
  def attributes(attr: String) = raw.getAttributes.findFirst(attr)
  
  override val isIMethod = Tri.lift(!is(Static))
  override val isFinal = Tri.lift(is(Final))
  def nat = raw.getNat
  lazy val dump = rop.dump
}
object Method extends Cacher[(Raw, RopMethod), Method] {
  private def intern(raw:Raw, rop:RopMethod) = cache.cache((raw, rop), new Method(raw, rop))
  def wrap(raw:Raw, rop:RopMethod) = cache cachedOrElse ((raw, rop), intern(raw, rop))
  implicit def unwrap(m:Method) = m.raw
}

final case class GhostMethod private (val spec:MethodSpec) extends DalvikMethodDesc {
  def prototype = spec.getPrototype
  def parent = GhostClass(spec.getDefiningClass)
  def nat = spec.getNat
}
object GhostMethod extends Cacher[MethodSpec, GhostMethod] {
  private def intern(spec:MethodSpec) = cache.cache (spec, new GhostMethod(spec))
  implicit def wrap(spec:MethodSpec) = cache cachedOrElse (spec, intern(spec))
  implicit def unwrap(gm:GhostMethod) = gm.spec
}

final case class ReflMethod private (private val refl: JMethod) extends MethodDesc {
  val parent = ReflClass(refl.getDeclaringClass())
  
  def argTs = for(klass <- argCs.get) yield Type(klass).asInstanceOf[Instantiable]
  def retT = Type(refl.getReturnType()).asInstanceOf[Instantiable]
  def argCs = Some(refl.getParameterTypes())
  
  def reflection = Some(refl)
}
object ReflMethod extends Cacher[JMethod, ReflMethod] {
  private def intern(refl:JMethod) = cache.cache (refl, new ReflMethod(refl))
  implicit def wrap(refl:JMethod) = cache cachedOrElse (refl, intern(refl))
  implicit def unwrap(gm:ReflMethod) = gm.refl
}