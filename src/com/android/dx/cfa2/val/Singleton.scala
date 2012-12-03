package com.android.dx.cfa2.`val`

trait Singleton extends Instantiable with Type.Singular {
  val singleton : Self#Instance
  final val defaultInst = singleton
  // There /is no "unknown"/
  final override def unknown(deps: Val_) = singleton
  // There is no construction per se
  protected[this] final val constructor = (params: IParams, deps: Val_)=>singleton
}