package com.android.dx.cfa2.prop

import com.android.dx
import dx.rop.code.AccessFlags
import dx.cfa2._
import tlc._

import collection.{Set => CSet, _}
import collection.{parallel => par}

/**
 * @see com.android.dx.rop.code.AccessFlags
 */
object Properties extends Enumeration {
  
    private val byBits = new MutableConcurrentMultiMap[Int, Property]
  
    sealed abstract class Property private[Properties] () extends Val() {
      val bitfield = {
        val label = "ACC"+getClass.getSimpleName.toUpperCase
        Class.forName("com.android.dx.rop.code.AccessFlags").getField(label).getInt()
      }
      byBits += (bitfield, this)
      import math._
      val bit = {
        val b = log(bitfield) / log(2)
        assert(b.toInt == b)
        b.toInt
      }
    }
    
    private[prop] def toProp(bits:Int) : Option[CSet[Property]] = byBits.get(bits)    
    private[prop] def toProps(rawFlags:Int, dom:Domain) : CSet[Property] = {
      val builder = new mutable.SetBuilder[Property, immutable.Set[Property]](immutable.Set())
	  for(shift <- 0 until 32) {
	    val bit = rawFlags & (1 << shift)
	    if(shift <= maxId && bit != 0)
	      toProp(bit) match {
	        case None        => throw new RuntimeException //TODO: Better error
	        case Some(props) => props.size match {
	          case 0 => throw new RuntimeException
	          case 1 => builder += props.head
	          case _ =>
	            val inter = props & dom
	            inter.size match {
	              case 1 => builder += inter.head
	              case _ => throw new RuntimeException
	            }
	        }
	      }
	    else {
	      // Unknown bits should be 0
	      if(bit != 0) throw new RuntimeException //TODO: Better error
	    }
	  }
	  builder result
    }
    
    sealed abstract class Access extends Property
    final class _Public private[Properties] () extends Access
    final class _Private private[Properties] () extends Access
    final class _Protected private[Properties] () extends Access
    import Algebra._
    type _Access = UNION [_Public]# | [_Private]# | [_Protected]# ^   
    val Public = new _Public()
    val Private = new _Private()
    val Protected = new _Protected()
    
    final class _Static private[Properties] () extends Property
    val Static = new _Static()
    
    final class _Final private[Properties] () extends Property
    val Final = new _Final()
    
    final class _Synchronized private[Properties] () extends Property
    val Synchronized = new _Synchronized()
    
    // This flag is unused by Dalvik
    final class _Super private[Properties] () extends Property
    val Super = new _Super()
    
    final class _Volatile private[Properties] () extends Property
    val Volatile = new _Volatile()
    
    final class _Bridge private[Properties] () extends Property
    val Bridge = new _Bridge()
    
    final class _Transient private[Properties] () extends Property
    val Transient = new _Transient()
    
    final class _Varargs private[Properties] () extends Property
    val Varargs = new _Varargs()
    
    final class _Native private[Properties] () extends Property
    val Native = new _Native()
    
    final class _Interface private[Properties] () extends Property
    val Interface = new _Interface()
    
    final class _Abstract private[Properties] () extends Property
    val Abstract = new _Abstract()
    
    final class _Strict private[Properties] () extends Property
    val Strict = new _Strict()
    
    final class _Synthetic private[Properties] () extends Property
    val Synthetic = new _Synthetic()
    
    final class _Annotation private[Properties] () extends Property
    val Annotation = new _Annotation()
    
    final class _Enum private[Properties] () extends Property
    val Enum = new _Enum()
    
    final class _Constructor private[Properties] () extends Property
    val Constructor = new _Constructor()
    
    final class _Declared_Synchronized private[Properties] () extends Property
    val DeclaredSynchronized = new _Declared_Synchronized()
    
}