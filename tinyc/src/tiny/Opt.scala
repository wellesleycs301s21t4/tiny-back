package tiny
import scala.collection.mutable.Map

/**
 * Optimization Framework
 * 
 * This stage optimizes code by recognizing simple redundancies or
 * computations that can be partially completed at compile time to save
 * time at run time.
 * 
 * We apply optimizations to code in the intermediate representation.  This
 * means that our optimizations are tied to neither the source language syntax
 * nor the destination language syntax.  (The semantics matter, but they are
 * handle by the translations into and out of the IR.)
 * 
 * Here, we implement an optimization as a function that takes and returns
 * instruction sequences (Seq[TacInstruction]).  It is tempting to build
 * one big, complicated, smart optimization function, but this is extremely
 * difficult in practice, in addition to being error-prone and typically
 * less effective than a simpler strategy.
 * 
 * We examine three classic optimizations: constant folding, copy propagation,
 * and dead code elimination.  Each is quite simple and easy to understand
 * (and only mildly useful) on its own.  Together, though, their impact is
 * much greater than that of the sum of the parts.
 * 
 * The apply method composes optimizations (applies them in sequence)
 * repeatedly taking the result and feeding it back until reaching a fixpoint,
 * (an input for which the optimizations produce the same output).
 * 
 * This architecture yields simple elegant code that is easy to understand in
 * stages, yet composes to do much more together.
 */
object Opt {
  /**
   * Compose the given set of optimizations and apply repeatedly until
   * reaching a fixpoint.
   */
  def apply(instructions: Seq[TacInstruction]) = {
		// FIXME: replace `once` with `fixpoint` for Exercise 16.
    once(instructions, pipeline(
				("Copy Prop",copyPropagate _)
				, ("Constant Fold",constantFold _)
				, ("Dead Code",deadCodeElim _)
				))
  }
  
  /**
   * Constant Folding
   * 
   * Compute the result of instructions whose operands are constant (literal)
   * and replace the instruction with a simple Copy of the result.
   * 
   * For example:
   *     v7 := 4 + 7
   * would be replaced by:
   *     v7 := 11
   *     
   * Algorithm: Map each instruction in the input to a new instruction
   * in the output.  Keep each instruction as is unless it meets the
   * constant-operands criterion -- replace those accordingly.
   * 
   * Use the map method and pattern-matching for this.  Only one kind of
   * instruction (with a certain kind of operands) will be replaced.
   * All others (use a wildcard case) are kept as is.
   */
	def constantFold(instructions: Seq[TacInstruction]) = {
		// FIXME: replace for Exercise 14.
		instructions.map (inst => inst)
	}
	
	/**
	 * Copy Propagation
	 * 
	 * Use the original source of a value instead of a copy.
	 * 
	 * If a source operand x is copied to a destination operand v then
	 * replace all later uses of v with x.
	 * 
	 * For example, transform this code:
	 *     v1 := 5
	 *     v2 := v1
	 *     v3 := v1 + v2
	 * to this code:
	 *     v1 := 5
	 *     v2 := 5
	 *     v3 := 5 + 5
	 *     
	 * Algorithm: Iterate through the instructions in order, keeping a map
	 * from each destination operand to the operand where its value originated
	 * (i.e., its replacement-to-be).
	 * For each instruction:
	 *   1. Replace the instruction with the same kind of instruction,
	 *      but with its source operands replaced according to the map.
	 *   2. Add an entry in the map, mapping the destination operand:
	 *      - If the instruction is a copy instruction,
	 *        then map the destination operand to the source operand.
	 *      - Otherwise, map the destination operand to itself.
	 *      
	 * We provide a mutable map and accessors for it.  Use the map method to
	 * do iteration and instruction replacement in one pass.  It will do the
	 * mappings in order.  Use pattern-matching to distinguish between
	 * kinds of instructions.
	 * 
	 * Alternatively, remove the mutable map and use folding to build both the
	 * result sequence and the map without mutation.
	 */
	def copyPropagate(instructions: Seq[TacInstruction]) = {
	  // A local map from cell ID to the original source of its contents.
	  // This map indicates how to replace cells.
		val statusMap = Map[Int,TacOperand]()
		// Given an operand, lookup its replacement.
		def get(operand: TacOperand) = operand match {
		  // If it's a literal, just use it.
		  case TacLiteral(_) => operand
		  // If it's a cell, replace it with whatever its original source is.
		  case TacCell(x) => statusMap(x)
		}
		// Set the map entry for loc to operand.
		def set(loc: TacCell, operand: TacOperand) = statusMap(loc.id) = operand

		instructions.map (inst =>
			inst match {
				case InputInst(dest) => {
				  set(dest, dest)
				  inst
				}
				case CopyInst(dest,src) => {
				  set(dest,
				      src match {
				        case TacLiteral(_) => src
				        case TacCell(x) => get(TacCell(x))
				      })
				  CopyInst(dest,get(dest))
				}
				case AddInst(dest,src1,src2) => {
				  set(dest,dest)
				  AddInst(dest,get(src1),get(src2))
				}
				case PrintInst(src) => PrintInst(get(src)) 
		  })

	}
	
	/**
	 * Dead Code Elimination
	 * 
	 * Remove all instructions that will have no impact on the programs's
	 * result or observable behavior.
	 * 
	 * In other words, if the result of a non-I/O instruction is never
	 * transitively used by an output instruction, remove it.
	 * 
	 * For example, transform:
	 *     v3 := 4
	 *     v4 := 7
	 *     v5 := v3 + 2
	 *     print v4
	 *     <end of program>
	 * to:
	 *     v4 := 7
	 *     print v4
	 *     <end of program>
	 *     
	 * Algorithm: Iterate through the instructions in REVERSE, building
	 * a map indicating whether a cell was used as a source operand.
	 * For each instruction:
	 * - If the instruction is an I/O instruction:
	 *   1. Prepend it to the result sequence.
	 *   2. Mark its source cell (if any) used.
	 * - Otherwise, if the instruction is not an I/O instruction:
	 *   - If the instruction's destination is marked used:
	 *     1. Prepend the instruction to the result sequence.
	 *     2. Mark the instruction's source cell(s) (if any) as used.
	 *   - Otherwise omit the instruction in the result sequence and
	 *     do not update the used markings.
	 *     
	 * We provide a mutable map from Int cell ID to Boolean used status
	 * and mutable variable initially holding an empty immutable result vector.
	 * Use a the reverseIterator method and a for loop to iterate backwards.
	 * Use pattern-matching to distinguish types of instructions and handle
	 * the destinations/sources of each properly.
	 * Use the   element +: vector   operator to build a larger vector from
	 * an element and a smaller vector.  (Lists could also work.)
	 * 
	 * Alternatively, fold right with no mutation.
	 */
	def deadCodeElim(instructions: Seq[TacInstruction]) = {
	  // Map from cell ID to whether it is used.
	  val used = Map[Int,Boolean]().withDefaultValue(false)
	  // Result sequence, initially an empty vector.
	  var result = Vector[TacInstruction]()
	  for (inst <- instructions.reverseIterator) {
	    inst match {
	      case InputInst(dest) => result = inst +: result
	      case CopyInst(TacCell(x),src) => {
	        if (used(x)) {
	          src match {
	            case TacCell(y) => used(y) = true
	            case _ =>
	          }
	          result = inst +: result
	        }
	      }
	      case AddInst(TacCell(x),src1,src2) => {
	        if (used(x)) {
	          src1 match {
	            case TacCell(y) => used(y) = true
	            case _ =>
	          }
	          src2 match {
	            case TacCell(y) => used(y) = true
	            case _ =>
	          }
	          result = inst +: result
	        }
	      }
	      case PrintInst(src) => {
	        src match {
	          case TacCell(x) => {
	            used(x) = true
	          }
	          case _ =>
	        }
 	        result = inst +: result
	      }
	    }
	  }
	  result
	}
	
	/**
	 * Compute a fixpoint of f starting from in.
	 * 
	 * Starting with initial result in, keep applying f to its own result
	 * until its result is the same as its argument.
	 */
	def fixpoint[T](in: T, f: T => T): T = {
	  var prev = in
	  var next = in
	  do {
	    prev = next
	    next = f(prev)
	  } while (next != prev)
		next
	}
	def once[T](in: T, f: T => T): T = f(in)
	
	/**
	 * Compose a list of named functions into one function, with printing
	 * along the way.
	 */
	def pipeline[T](transforms: (String, Seq[T] => Seq[T])*)(in: Seq[T]) = {
	  var xs = in
	  for ((name,transform) <- transforms) {
	    println("---- " + name + " ----")
	    xs = transform(xs)
	    for (x <- xs) println("  " + x)
	  }
	  xs
	}
	
	/* Other optimizations:
	 * 0 * x = x * 0 = 0
	 * 0 + x = x + 0 = x
	 */

	// Mention register/stack allocation for minimizing space.
}