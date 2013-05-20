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

sealed trait MethodDesc extends Immutable with NotNull {
  def name: String
  def parent: ClassDesc
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
  final def exnTs: Option[Seq[Exceptional]] = exnCs match {
    case None     => None
    case Some(cs) =>
      Some(for(c <- cs) yield Type(c).asInstanceOf[Exceptional]) 
  }
  
  def argCs : Option[Seq[JClass[_]]]
  def exnCs : Option[Seq[JClass[_]]]
  
  def reflection: Option[JMethod]
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
  
  final def isFinal = is(Final) | ((parent is Final) == Tri.T)
  final def isIMethod = !is(Static)
  
  final def is(prop: Property) = {
    assert(Domain.Method contains prop)
    _is(prop)
  }
  def _is(prop: Property): Tri
  
  final val isAccessible = parent.isAccessible & {
    (this is Properties.Public) |
    ((this is Properties.Protected) & !(this is Final))
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
  final lazy val exnCs= reflection match {
    case None       => None
    case Some(refl) => Some(refl.getExceptionTypes().toSeq)
  }
  
  final lazy val reflection = parent.typ match {
    // Arrays can have methods, too
    case typ:RefType => typ.klass match {
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
  def _is(prop: Property) = props contains prop
  lazy val parent = GhostClass(raw.getDefiningClass)
  def accessFlags = raw.getAccessFlags
  // FIXME: We need to exclude the "this" so we can vary on it; also lines up with Ghost and Refl
  def prototype = raw.getEffectiveDescriptor
  def attributes(attr: String) = raw.getAttributes.findFirst(attr)
  
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
  lazy val parent = GhostClass(spec.getDefiningClass)
  def nat = spec.getNat
  lazy val props = reflection match {
    case None       => null
    case Some(refl) => Domain.Method.fromJMember(refl)
  }
  def _is(prop: Property) =
    if(props == null) Tri.U
    else props(prop)
}
object GhostMethod extends Cacher[MethodSpec, GhostMethod] {
  private def intern(spec:MethodSpec) = cache.cache (spec, new GhostMethod(spec))
  implicit def wrap(spec:MethodSpec) = {
    // Upgrade to ReflMethod if we can
    val ghost = cache cachedOrElse (spec, intern(spec))
    ghost.reflection match {
      case Some(refl) => ReflMethod.wrap(refl)
      case None       => ghost
    }
  }
  implicit def unwrap(gm:GhostMethod) = gm.spec
}

sealed trait ReflMethodDesc extends MethodDesc {
  protected val refl: JMember
  final def name = refl.getName
  final lazy val parent = ReflClass(refl.getDeclaringClass())
  
  final def argTs = for(klass <- argCs.get) yield Type(klass).asInstanceOf[Instantiable]
  
  final lazy val props = Domain.Method.fromJMember(refl)
  final def _is(prop) = props(prop)
}

final case class ReflMethod private (protected[this] val refl: JMethod) extends ReflMethodDesc {
  def retT = Type(refl.getReturnType()).asInstanceOf[Instantiable]
  def argCs = Some(refl.getParameterTypes().toSeq)
  def exnCs = Some(refl.getExceptionTypes())
  
  def reflection = Some(refl)
}
object ReflMethod extends Cacher[JMethod, ReflMethod] {
  private def intern(refl:JMethod) = cache.cache (refl, new ReflMethod(refl))
  implicit def wrap(refl:JMethod) = cache cachedOrElse (refl, intern(refl))
  implicit def unwrap(gm:ReflMethod) = gm.refl
}

final case class ReflConstructor private (protected[this] val refl: JConstructor[_]) extends ReflMethodDesc {
  def retT = Type(refl.getDeclaringClass()).asInstanceOf[Instantiable]
  def argCs = Some(refl.getParameterTypes())
  def exnCs = Some(refl.getExceptionTypes())
  
  def reflection = None
}