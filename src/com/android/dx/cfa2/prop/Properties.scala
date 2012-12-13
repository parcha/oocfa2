package com.android.dx.cfa2.prop

import com.android.dx
import dx.rop.code.AccessFlags
import dx.cfa2._

import collection.{Set => CSet, _}
import collection.{parallel => par}

import java.lang.reflect.{Member => JMember, Modifier => JModifier, Method => JMethod}

/**
 * @see com.android.dx.rop.code.AccessFlags
 */
object Properties extends Enumeration {
  
    // TODO: Give this a more proper type
    private lazy val byBits = {
      val build = new MutableConcurrentMultiMap[Int, Property]
      for(v <- values) {
        val prop = v.asInstanceOf[Property]
        build += (prop.bitfield, prop) 
      }
      build
    }
  
    sealed class Property private[Properties] (name: String) extends Val(name) {
      final lazy val bitfield = {
        val label = "ACC_"+this.toString.toUpperCase
        Class.forName("com.android.dx.rop.code.AccessFlags").getField(label).getInt()
      }
      import math._
      final lazy val bit = {
        val b = log(bitfield) / log(2)
        assert(b.toInt == b)
        b.toInt
      }
    }
    
    sealed trait Reflectable extends Property {
      def testJMember(m: JMember): Boolean
    }
    
    sealed trait JModifierReflectable extends Reflectable {
      final def testJMember(m) = tester(m.getModifiers())
      final def testJModifiers(mods: Int) = tester(mods)
      protected[this] val tester: (Int => Boolean)
    }
    
    private[prop] def toProp(bits:Int) : Option[CSet[Property]] = byBits.get(bits)    
    private[prop] def toProps(rawFlags:Int, dom:Domain) : CSet[Property] = {
      val builder = new mutable.SetBuilder[Property, immutable.Set[Property]](immutable.Set())
	  for(shift <- 0 until 32) {
	    val bit = rawFlags & (1 << shift)
	    if(shift <= maxId && bit != 0)
	      toProp(bit) match {
	        case None        => throw new InternalError(s"Could not find $bit in $byBits")
	        case Some(props) => props.size match {
	          case 0 => throw new InternalError
	          case 1 => builder += props.head
	          case _ =>
	            val inter = props & dom
	            inter.size match {
	              case 1 => builder += inter.head
	              case _ => throw new InternalError
	            }
	        }
	      }
	    else {
	      // Unknown bits should be 0
	      if(bit != 0) throw new InternalError //TODO: Better error
	    }
	  }
	  builder result
    }
    
    sealed class Access private[Properties] (name: String) extends Property(name)
    //import tlc.Algebra._
    //type _Access = UNION [Public.type]# | [Private.type]# | [Protected.type]# ^   
    val Public = new Access("Public") with JModifierReflectable { val tester = JModifier.isPublic _ }
    val Private = new Access("Private") with JModifierReflectable { val tester = JModifier.isPrivate _ }
    val Protected = new Access("Protected") with JModifierReflectable { val tester = JModifier.isProtected _ }
    
    val Static = new Property("Static") with JModifierReflectable { val tester = JModifier.isStatic _ }
    
    val Final = new Property("Final") with JModifierReflectable { val tester = JModifier.isFinal _ }
    
    val Synchronized = new Property("Synchronized") with JModifierReflectable { val tester = JModifier.isSynchronized _ }
    
    // This flag is unused by Dalvik
    val Super = new Property("Super")
    
    val Volatile = new Property("Volatile") with JModifierReflectable { val tester = JModifier.isVolatile _ }
    
    val Bridge = new Property("Bridge") with Reflectable {
      def testJMember(m) = m match {
        case m: JMethod => m.isBridge
      }
    }
    
    val Transient = new Property("Transient") with JModifierReflectable { val tester = JModifier.isTransient _ }
    
    val Varargs = new Property("Varargs") with Reflectable {
      def testJMember(m) = m match {
        case m: JMethod => m.isVarArgs
      }
    }
    
    val Native = new Property("Native") with JModifierReflectable { val tester = JModifier.isNative _ }
    
    val Interface = new Property("Interface") with JModifierReflectable { val tester = JModifier.isInterface _ }
    
    val Abstract = new Property("Abstract") with JModifierReflectable { val tester = JModifier.isAbstract _ }
    
    val Strict = new Property("Strict") with JModifierReflectable { val tester = JModifier.isStrict _ }
    
    val Synthetic = new Property("Synthetic") with Reflectable {
      def testJMember(m) = m match {
        case m: JMethod => m.isSynthetic
      }
    }
    
    val Annotation = new Property("Annotation")
    
    val Enum = new Property("Enum") with JModifierReflectable { val tester = JModifier.isStatic _ }
    
    val Constructor = new Property("Constructor")
    
    val DeclaredSynchronized = new Property("Declared_Synchronized")
    
}