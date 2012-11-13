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

sealed trait MethodDesc extends Immutable with NotNull {
  final def name = nat.getName.getString
  final lazy val id = {
    val pname = parent.typ.toHuman
    IDParsers.parse(IDParsers.meth_id, pname+"."+name) match {
      case IDParsers.Success(id, _) => id
      case _ => throw new RuntimeException("Failure in parsing the method descriptor for "+pname+"."+name)
    }
  }
  def prototype: dx.rop.`type`.Prototype
  final lazy val arity = argTs.size
  final lazy val argTs =
    for(i <- 0 until prototype.getParameterTypes.size)
      // TODO: Should this be ParameterFrameTypes?
      yield Type(prototype.getParameterTypes.get(i)).asInstanceOf[Instantiable]
  final lazy val retT = Type(prototype.getReturnType).asInstanceOf[Instantiable]
  
  // Reflection support
  import java.lang.reflect
  final lazy val argCs : Option[Seq[java.lang.Class[_]]] =
    if(_argCs contains None) None
    else Some(_argCs.flatten)
  /** More primitive argCs that lets one see which args in particular failed **/
  final lazy val _argCs : Seq[Option[java.lang.Class[_]]] =
    for(i <- 0 until arity;
        val t = prototype.getParameterTypes.get(i))
      yield
        Type(t).asInstanceOf[Instantiable].klass match {
          case null  => None
          case klass => Some(klass)
        }
  final lazy val reflection: Option[reflect.Method] = parent.typ match {
    case typ:OBJECT => typ.klass match {
      case null => None
      case klass => argCs match {
        case None  => None
        case Some(argCs) => try {
          //CFA2Analysis.singleton.opts.log('debug) ("Seeking reflection for "+name)
          val refl = klass.getDeclaredMethod(name, argCs:_*)
          //CFA2Analysis.singleton.opts.log('debug) ("Found as "+refl)
          Some(refl)
        }
        catch {
          case e:Exception => None
          // Hack until we can get the classloader to cooperate
          case e:NoClassDefFoundError => None
        }
      }
    }
  }
  // FIXME: Need to figure out how to always determine whether it's an instance method or not
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
  
  def parent: ClassDesc
  def nat: dx.rop.cst.CstNat
  final lazy val isInstanceInit = nat.isInstanceInit
  final lazy val isClassInit = nat.isClassInit
  
  val isIMethod: Tri
  
  override lazy val toString = reflection match {
    case Some(refl) => refl toString
    case None       => id+"/"+arity
  }
}

final case class Method private (val raw:Raw, val rop:RopMethod) extends MethodDesc {
  lazy val blocks: BasicBlockSet = {
    val bbs = rop.getBlocks
    new BasicBlockSet(
      (for(i <- 0 until bbs.size) yield BasicBlock.wrap(bbs.get(i), this)).toSet)
  }
  def firstBlock = blocks(rop.getFirstLabel)
  
  lazy val props = prop.Range(prop.Domain.Method, accessFlags)
  def is(prop: Property*) = props contains(prop:_*)
  lazy val parent = GhostClass(raw.getDefiningClass)
  def accessFlags = raw.getAccessFlags
  def prototype = raw.getEffectiveDescriptor
  def attributes(attr: String) = raw.getAttributes.findFirst(attr)
  
  val isIMethod = Tri.lift(!is(Static))
  def nat = raw.getNat
  lazy val dump = rop.dump
}
object Method extends Cacher[(Raw, RopMethod), Method] {
  private def intern(raw:Raw, rop:RopMethod) = cache.cache((raw, rop), new Method(raw, rop))
  def wrap(raw:Raw, rop:RopMethod) = cache cachedOrElse ((raw, rop), intern(raw, rop))
  implicit def unwrap(m:Method) = m.raw
}

final case class GhostMethod private (val spec:MethodSpec) extends MethodDesc {
  // FIXME: we need a way to "getEffectiveDescriptor" here...
  def prototype = spec.getPrototype
  def parent = GhostClass(spec.getDefiningClass)
  def nat = spec.getNat
  val isIMethod = Tri.U
}
object GhostMethod extends Cacher[MethodSpec, GhostMethod] {
  private def intern(spec:MethodSpec) = cache.cache (spec, new GhostMethod(spec))
  implicit def wrap(spec:MethodSpec) = cache cachedOrElse (spec, intern(spec))
  implicit def unwrap(gm:GhostMethod) = gm.spec
}