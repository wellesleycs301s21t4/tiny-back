package tiny

import java.io.FileWriter
import scala.collection.mutable.Map
import scala.sys.process._

/**
 * Wrapper for the assembler and linker.
 * This takes our generated assembly code file, links it with a library
 * for runtime I/O support, and produces a binary machine code file
 * (an executable).
 */
object Link {
  def apply(asm: String, asmpath: String, binfile: String) = {
		val home = System.getenv("TINY_HOME")
		val runtimesrc = home + "/src/main/c/runtime/runtime.c"
		val runtimeobj = home + "/target/runtime.o"
		val runtimeResult = Seq("gcc", "-g", "-c", "-o", runtimeobj, runtimesrc).!
		if (runtimeResult != 0) throw new LinkError("failed to build runtime library")

		println(asmpath)
		val asmfile = new FileWriter(asmpath)
		asmfile.write(asm)
		asmfile.close

		println(binfile)
		val linkResult = Seq("gcc", "-g", "-o", binfile, asmpath, runtimeobj).!
		if (linkResult != 0) throw new LinkError("failed to link executable")
  }
}

class LinkError(msg: String) extends CompilerError(msg)