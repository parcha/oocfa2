package com.android.dx.cfa2
package env

import wrap._
import `val`._
import Heap._
import tlc.Algebra._

import scala.reflect._
import scala.reflect.runtime.universe._

/* The idiom for defining a new kind of heap is as follows:
 * * Define its scheme.  As a trait if you're defining the scheme for a regioned heap, this way it
 *   can be mixed in for regions' schemes; as an abstract class if you're defining the scheme for
 *   a leaf heap.  Extend from a RegionScheme derivative if you're talking about a region of
 *   an existing kind of heap.  Don't forget to extend that scheme in this case.
 * * Define the singleton object alongside the scheme which defines the state (vs. the types)
 *   for that scheme, when used in the concrete.  In particular, the object must hold the
 *   WeakTypeTag of its Address in _Address.
 * * Define the heap, using the trait/class for the scheme everywhere its type is called for;
 *   only use the concrete object as the implicit scheme value that needs to be passed in. 
 */

trait TopHeapScheme extends HeapScheme {
  sealed abstract class Address
  abstract class ObjAddr[_ <: RefType] protected[TopHeapScheme] extends Address
  /** Check for this specially; no region will contain it **/
  final case object NullAddr extends ObjAddr[NULL.type]
  case class SFAddr protected[TopHeapScheme] (raw:FieldSlot) extends Address
  
  // Referent values are guaranteed to not be empty
  //type Referent = Any
}
object TopHeapScheme extends TopHeapScheme {
  implicit val _Address: WeakTypeTag[Address] = weakTypeTag[Address]
}

class TopHeap(regions: Regions[TopHeapScheme])
extends Shell[TopHeap, TopHeapScheme](regions)(TopHeapScheme) {
  protected[this] def constructor(regions) = new TopHeap(regions)
}


abstract class SFHeapScheme
extends LeafScheme[SFHeapScheme, TopHeapScheme] with TopHeapScheme {
  type Address = SFAddr
  type Referent = Val[ValuedType[_]] ||| ObjAddr[_]
}
object SFHeapScheme extends SFHeapScheme {
  implicit val _Address: WeakTypeTag[Address] = weakTypeTag[Address]
}

class SFHeap(proxy:SFHeapScheme#Map_)
extends Leaf[SFHeap, SFHeapScheme](proxy)(SFHeapScheme) {
  protected[this] def constructor(proxy) = new SFHeap(proxy)
}


abstract class ObjHeapScheme
extends WrappedLeafScheme[ObjHeapScheme, TopHeapScheme] with TopHeapScheme {
  type Address = ObjAddr
  type Referent <: Val[OBJECT]
  
  case class ObjAddr protected[ObjHeapScheme] (raw:Address_) extends super.ObjAddr[OBJECT]
  implicit def unwrapAddress(addr) = addr.raw
  implicit def wrapAddress(raw) = ObjAddr(raw)
}
object ObjHeapScheme extends ObjHeapScheme {
  implicit val _Address: WeakTypeTag[Address] = weakTypeTag[Address]
  type Address_ = Int
}

class ObjHeap(proxy:ObjHeapScheme#Map_)
extends WrappedLeaf[ObjHeap, ObjHeapScheme](proxy)(ObjHeapScheme) {
  protected[this] def constructor(proxy) = new ObjHeap(proxy)
}