package com.android.dx.cfa2

import scala.collection._

trait JSONOpts {
  val rawCfg: Map[String, Any]
  
  protected final def param__[T](p: String, default: T)
  (implicit T_ : Manifest[T]): T = param_[T](p) match {
    case None    => default
    case Some(v) => v
  }
  protected final def param__[T,U](p: String, default: U, f: T=>Either[U,String])
  (implicit T_ : Manifest[T]): U = param_[T](p) match {
    case None    => default
    case Some(v) => f(v) match {
      case Left(u)    => u
      case Right(msg) =>
        onConfigurationError("Failed to validate configuration parameter "+p+":\n"+msg)
    }
  }
      
  protected final def param_[T](p: String)
  (implicit T_ : Manifest[T]): Option[T] = rawCfg get p match {
    case None      => None
    case Some(v:T) => Some(v)
    case Some(_)   =>
      onConfigurationError("Optional configuration value for "+p+" must be of type "+T_)
  }
  protected final def param_[T,U](p: String, f: T=>Either[U,String])
  (implicit T_ : Manifest[T]): Option[U] = param_[T](p) match {
    case None      => None
    case Some(v)   => f(v) match {
      case Left(u)    => Some(u)
      case Right(msg) =>
        onConfigurationError("Failed to validate configuration parameter "+p+":\n"+msg)
    }
  }
      
  protected final def param[T](p: String)(implicit T_ : Manifest[T]): T = rawCfg get p match {
    case Some(v:T) => v
    case Some(_)   =>
      onConfigurationError("Configuration value for "+p+" must be of type "+T_)
    case None      =>
      onConfigurationError("Configuration file expected to specify "+p+"  of type "+T_)
  }
  protected final def param[T,U](p: String, f: T=>Option[U])
  (implicit T_ : Manifest[T], U_ : Manifest[U]): U = f(param[T](p)) match {
    case Some(u) => u
    case None    =>
      onConfigurationError("Configuration value for "+p+" must be convertible to "+U_)
  }
  
  protected def onConfigurationError(msg: String) =
    throw new RuntimeException(msg)
}