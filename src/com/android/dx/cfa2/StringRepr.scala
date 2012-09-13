package com.android.dx.cfa2

import com.android.dx
import dx.cfa2._
import scala.collection._

trait HasLinearizedRepr {
  protected def linearizedRepr : GenIterable[String]
  protected val twixtLines : String = "\n"
  override def toString : String = linearize(linearizedRepr, twixtLines)
}

trait HasColumnizedRepr {
  protected implicit def columnizedRepr : Matrix[String]
  protected val colSpacing : Int = 2 
  protected val twixtCols : String = "\t"
  protected val twixtRows : String = "\n"
  override def toString : String =
    linearizeMat(normalize(columnizedRepr, colSpacing), twixtCols=twixtCols, twixtRows=twixtRows)
}

trait PrettyMap[K,V] extends HasColumnizedRepr { _:GenMap[K,V] =>
  protected final def columnizedRepr : Matrix[String] =
    for((k,v) <- this) yield linearRepr(k,v)
  override protected val twixtCols = "=>"
  protected def linearRepr(k:K, v:V) = immutable.IndexedSeq(keyStr(k), valueStr(v))
  protected def keyStr(k:K) : String = k.toString
  protected def valueStr(v:V) : String = v.toString
  override def toString = if(isEmpty) (immutable.Map[K,V]()).toString
                          else        super.toString
}