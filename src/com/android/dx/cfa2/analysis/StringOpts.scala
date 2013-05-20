package com.android.dx.cfa2.analysis

import scala.collection._
import scala.reflect.runtime.universe._
import scala.reflect.api.TypeTags

trait StringOpts {
  protected[this] val rawCfg: Map[String, Any]
  
  protected final def param__[T : TypeTag](p: String, default: T): T = param_[T](p) match {
    case None    => default
    case Some(v) => v
  }
  protected final def param__[T: TypeTag, U: TypeTag](p: String, default: U, f: T=>Either[U,String]): U =
    param_[T](p) match {
      case None    => default
      case Some(v) => f(v) match {
        case Left(u)    => u
        case Right(msg) =>
          onConfigurationError("Failed to validate configuration parameter "+p+":\n"+msg)
      }
    }
      
  protected final def param_[T: TypeTag](p: String): Option[T] = rawCfg get p match {
    case None      => None
    case Some(v:T) => Some(v)
    case Some(_)   =>
      onConfigurationError("Optional configuration value for "+p+" must be of type "+typeOf[T])
  }
  protected final def param_[T: TypeTag, U: TypeTag](p: String, f: T=>Either[U,String]): Option[U] =
    param_[T](p) match {
      case None      => None
      case Some(v)   => f(v) match {
        case Left(u)    => Some(u)
        case Right(msg) =>
          onConfigurationError("Failed to validate configuration parameter "+p+":\n"+msg)
      }
    }
      
  protected final def param[T: TypeTag](p: String): T = rawCfg get p match {
    case Some(v:T) => v
    case Some(_)   =>
      onConfigurationError("Configuration value for "+p+" must be of type "+typeOf[T])
    case None      =>
      onConfigurationError("Configuration file expected to specify "+p+"  of type "+typeOf[T])
  }
  protected final def param[T: TypeTag, U: TypeTag](p: String, f: T=>Option[U]): U =
    f(param[T](p)) match {
      case Some(u) => u
      case None    =>
        onConfigurationError("Configuration value for "+p+" must be convertible to "+typeOf[U])
    }
  
  protected def onConfigurationError(msg: String) =
    throw new InternalError(msg)
}