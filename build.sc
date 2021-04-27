import mill._
import mill.scalalib._
import scala.sys.process._

object tinyc extends ScalaModule {
  def tinyPath = millSourcePath / "src" / "tiny"
  def binPath = os.pwd / "bin"
  def tinycPath = binPath / "tinyc"
  def tinyiPath = binPath / "tiny"

  def scalaVersion = "2.12.8"
  override def ivyDeps = Agg(ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
                             ivy"${scalaOrganization()}:scala-compiler:${scalaVersion()}")

  def script = T {
    val classpath = runClasspath().map(_.path).mkString(":")
    os.makeDir.all(binPath)
    val tinycCode = "#!/bin/bash\n# THIS FILE IS AUTO GENERATED\nexec java -classpath " + classpath + " tiny.Compiler \"$@\""
    os.write.over(tinycPath, tinycCode)
    os.proc("chmod", "+x", tinycPath).call()
    val tinyiCode = "#!/bin/bash\n# THIS FILE IS AUTO GENERATED\nexec java -classpath " + classpath + " tiny.Interpreter \"$@\""
    os.write.over(tinyiPath, tinyiCode)
    os.proc("chmod", "+x", tinyiPath).call()
    tinycCode
  }
  
  def build = T {
    compile()
    script()
  }
}
