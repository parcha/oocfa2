package com.android.dx.cfa2

import com.android.dx
import dx.cfa2._
import parsers._
import dx.rop.`type`.{Type => DType}
import java.lang.{Class => JClass}
import java.net.{URLClassLoader, URL}

class AnalysisClassLoader(parent: ClassLoader, urls: Iterable[URL] = List())
extends URLClassLoader(urls.toArray, parent) {
  
  def reflectClass(name: String) : Option[JClass[_]] =
    try Some(loadClass(name))
    catch {
      case _:ClassNotFoundException =>
        warn("Couldn't find class for reflection: "+name)
        None
      case e:NoClassDefFoundError =>
        warn("In attempting to load "+name+" for reflection, a depedency could not be found: "+e.getMessage)
        None
    }
  def reflectClass(dalvikType: DType) : Option[JClass[_]] = reflectClassDescriptor(dalvikType.getDescriptor)
  def reflectClassDescriptor(desc: String) : Option[JClass[_]] = {
    val descriptorMatch = (descriptorRegex findFirstMatchIn desc).get
    descriptorMatch.group("classname") match {
      case null =>
        CFA2Analysis.log('debug) (s"No classname found for descriptor ${desc}")
        None
      case cname =>
        reflectClass(cname.replace('/', '.'))
    }
  }
  
  private[cfa2] def +=(url: URL) = super.addURL(url)
  
  private[cfa2] def registerRawClass(name: String, data: Array[Byte]) =
    defineClass(name, data, 0, data.length)
  
  protected[this] def warn(msg:String) =
    // FIXME: HACK: for now, we just pull the singleton and warn that way
    CFA2Analysis.log('warn)(msg)
}