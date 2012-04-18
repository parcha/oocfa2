import sbt._

class ScalaSMT(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = 
    CompileOption("-Xexperimental") :: 
    Unchecked ::
    Deprecation :: 
    super.compileOptions.toList

  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1.RC1" % "test"
}
