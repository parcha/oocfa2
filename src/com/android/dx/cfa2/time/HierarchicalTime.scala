package com.android.dx.cfa2.time

import scala.ref._

/**
 *  For hierarchical timekeeping
 */
  sealed trait HierarchicalTime extends Time {
    protected[this] val parent_ : WeakReference[Time]
    final def parent = {
      val p : Time = parent_()
      /*
       * The child should not be usable if the parent is not usable
       */
      require(p != null)
      p
    }
    
    import Time.CMP._
    abstract override def cmp(t: Time) = t match {
      case t_ :HierarchicalTime => t_.parent cmp parent match {
        case EQ  => topcmp(t)
        case cmp => cmp
      }
      case _                    => topcmp(t)
      /*topcmp(t) match {
        case NC  => relationTo(t) match {
          case null => NC
          case r    => topcmp(r)
        }
        case cmp => cmp
      }*/
    }
    protected def topcmp(t: Time) = super.cmp(t)
    
    /**
     * Find out if we are a parent to t, and if so, return the immediate child
     */
    def relationTo(t: Time) : Time = t match {
      case null                 => null
      case t_ :HierarchicalTime => if(t_.parent == parent) t_ else relationTo(t_)
      case _                    => if(t == this) t else null
    }
    
    def sameLineage(t: HierarchicalTime) : Boolean =
      if(t.parent eq parent)
        parent match {
        case null                  => true
        case p_ : HierarchicalTime => p_ sameLineage t.parent.asInstanceOf[HierarchicalTime]
        case _                     => true
        }
      else false
  }
  
  final class HLinearTime(parent : Time) extends LinearTime with HierarchicalTime {
    protected[this] val parent_ = new WeakReference(parent)
  }
  final class HUnitTime(parent : Time) extends UnitTime with HierarchicalTime {
    protected[this] val parent_ = new WeakReference(parent)
  }
