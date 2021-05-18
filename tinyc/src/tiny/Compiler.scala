package tiny

import java.util.Scanner
import java.io.File

/**
  * A Tiny Calculator Language Compiler
  *
  * Our compiler translates calculator source code to x86 machine code
  * in stages using multiple internal and external program representations.
  *
  * External program representations:
  * - Tiny source code
  * - x86 assembly language code
  * - x86 machine code
  *
  * Internal program representations:
  * - AST.scala: Abstract Syntax Tree
  * - IR.scala: Intermediate Representation/Three-Address Code
  *
  * Compilation stages:
  * - Parse.scala: Lexing and Parsing, from source to AST
  * - ScopeCheck.scala: Scope checking
  * - IRGen.scala: IR Generation, from AST to IR
  * - Opt.scala: Optimization, from IR to IR
  * - CodeGen.scala: Code Generation, from IR to x86 assembly
  * - Link.scala: [External] Assembly and Linking, from assembly to machine code executable
  *
  */
object Compiler {
  def main(args: Array[String]): Unit = {
    try {
      val source = if (args.isEmpty) new Scanner(System.in) else new Scanner(new File(args(0)))

      // Front End steps: from input to AST.
      val ast = frontend(source)

      // Back End steps: from AST to executable.
      backend(ast, args)

      println("================")
      println("ACCEPTED")
    }
    // Comment this out to see where spurious errors originate.
    catch {
      // Die with compiler errors.
      case e: CompilerError => {
        println("Tiny " + e.getClass.getSimpleName + ": " + e.getMessage)
        println("================")
        println("REJECTED")
      }
    }
  }

  def frontend(source: Scanner): Program = {
    // 0. PARSE
    // Parse the source code character sequence to an Abstract Syntax Tree.
    println("== AST ===========")
    val ast = Parse(source)
    println(ast)

    // 1. CHECK
    // Check variable uses to ensure they follow definitions respecting scope.
    println("== CHECK =========")
    ScopeCheck(ast)

    // Return the checked AST.
    ast
  }

  def backend(ast: Program, args: Array[String]): Unit = {
    // 2. IR
    // Convert the AST to a Three-Address Code Intermediate Representation.
    println("== IR ===========")
    val ir = IRGen(ast)
    for (inst <- ir) println("  " + inst)

    // 3. OPTIMIZE
    // Optimize the IR program.
    // FIXME: uncomment the next two lines for tiny-back Exercise 12:
    // println("== OPT ===========")
    // val irOpt = Opt(ir)
    // FIXME: remove the next line for tiny-back Exercise 12:
    val irOpt = ir


    /* FIXME: uncomment this section for tiny-back Exercise 10.

    // 4. CODEGEN
    // Translate the TAC into x86 assembly code.
    println("== ASM ===========")
    val asm = CodeGen(irOpt)
    println(asm)

    // 5. LINK
    // [External] Assemble the generated code and link a binary executable.
    println("== BIN ===========")
    if (args.isEmpty) Link(asm, "stdin.s", "stdin.bin")
    else Link(asm, args(0) + ".s", args(0) + ".bin")

    END FIXME section for tiny-back Exercise 10 */
  }
}

/** Common basis for various types of compiler errors. */
class CompilerError(msg: String) extends Exception(msg)

