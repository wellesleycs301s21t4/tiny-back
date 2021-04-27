package tiny

/**
 * Intermediate Representation
 * 
 * An intermediate representation is a representation of programs
 * somewhere between the abstract syntax (AST) and assembly code for
 * a specific computer architecture.  ASTs and assembly code are quite
 * far apart in abstraction levels.  Assembly code lacks, for example,
 * the notion of expressions or variables as available in the source
 * language.  The source language has no concept of registers,
 * instructions, or memory locations, the primitives of assembly language.
 * (A "real" programming language would also have structured control-flow,
 * which is missing in assembly language's jumps and branches.)
 * ASTs for the source language and representations of the assembly
 * language are both very specific (i.e., not general).
 * 
 * An Intermediate Representation is a language is a stepping stone
 * in between.  It can also be very general, not tied to one particular
 * source language or machine language, thereby supporting many different
 * "front ends" and "back ends".  Most compiler optimizations work on IR
 * for this reason.
 * 
 * Typically, IRs represent some sort of abstract machine with infinitely
 * many storage locations and a set of instructions that manipulate the
 * values stored in these locations.  It is closer to real machine code,
 * but not tied to any details of a specific computer architecture.
 * 
 * We use a simple Three-Address Code (TAC), so called because each
 * instruction may use no more than three addresses (of abstract storage
 * locations) at a time.  In addition to storage locations, our TAC also
 * supports integer literals.  It supports a small number of basic
 * instructions.
 */

/** Operands are either storage locations or literals. */
abstract class TacOperand
/** An abstract storage cell with a unique ID. */
case class TacCell(id: Int) extends TacOperand {
  	override def toString = "x" + id
}
/** A literal integer value. */
case class TacLiteral(value: Int) extends TacOperand {
  	override def toString = value.toString
}

/**
 * There are four types of instructions.  Most produce a result and store
 * it in a destination location, given by the optional destination here.
 */
abstract class TacInstruction(val dest: Option[TacCell])
/**
 * Add instructions read their two source operands, add the values, and
 * store the result in the destination location.
 */
case class AddInst(dst: TacCell, src1: TacOperand, src2: TacOperand) extends TacInstruction(Some(dst)) {
  	override def toString = dst + " := " + src1 + " + " + src2
}
/**
 * Copy instructions read the source operand and store its value in the
 * destination location.
 */
case class CopyInst(dst: TacCell, src: TacOperand) extends TacInstruction(Some(dst)) {
  	override def toString = dst + " := " + src
}
/**
 * Input instructions read a value from user input and store the result
 * in the destination location.
 */
case class InputInst(dst: TacCell) extends TacInstruction(Some(dst)) {
  	override def toString = dst + " := input"
}
/**
 * Print instructions read a source operand and print this value as output.
 * They to do not store a result and therefore lack a destination.
 */
case class PrintInst(src: TacOperand) extends TacInstruction(None) {
  	override def toString = "print " + src
}