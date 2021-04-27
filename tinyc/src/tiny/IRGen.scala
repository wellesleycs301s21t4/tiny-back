package tiny
import scala.collection.mutable.Map

/**
 * Translation from Abstract Syntax to Intermediate Representation
 * 
 * Just as the parser was composed of recursive functions that built up
 * the recursive AST structure, functions that consume or tear down the AST
 * will follow similar recursive structure.
 * 
 * Symbol Tables
 * To record information about each variable, we used a symbol table.
 * The translation from AST to IR involves mapping variables in the
 * source code to abstract storage cells in the IR.  It also uses
 * the symbol table to maintain and query this mapping.
 * 
 * Translating ASTs to IR
 * Translation proceeds by doing a post-order traversal of the AST.
 */
object IRGen {
  /**
   * Translates the AST to three-address code IR starting from the root.
   */
	def apply(ast: Program) = {
		translateProgram(ast)
	}
  
	/**
	 * A fresh-variable generator.  Call fresh() to get a unique TacCell.
	 */
	var id = 0
	def fresh() = {
		id = id + 1
		TacCell(id)
	}

	/**
	 * Translate an expression AST to a sequence of TacInstructions,
	 * using the given symbol table.
	 * 
	 * Translate an expression by doing a post-order traversal of the AST:
	 * 1. Generate the instructions that produce the result of the
	 *    subexpressions of an expression.
	 * 2. Find where the results of subexpressions are stored in the IR by
	 *    checking the destination of the last instruction in their translation.
	 * 3. Then, use these cells as the source operands of an instruction that
	 *    implements the expression.
	 * 4. Finally, concatenate all the translated instructions in the right
	 *    order for the result.
	 * You can say all this in many fewer lines in Scala...
	 */
	def translateExpr(expr: Expr, 
	              symtab: Map[String,TacCell]): Seq[TacInstruction] = {
		expr match {
		  // Input expressions are translated by emitting an Input instruction
		  // whose destination is a fresh cell.
  		case Input() => Vector(InputInst(fresh()))
  		// FIXME: implement remaining cases.
		}
	}
	
	/**
	 * Translate a statement AST to a sequence of TacInstructions,
	 * using the given symbol table.
	 */
	def translateStmt(stmt: Stmt, 
                symtab: Map[String,TacCell]): Seq[TacInstruction] = {
	  stmt match {
	    // Print statement: translate the source expression to TAC,
	    // then emit a print instruction whose source operand is the
	    // destination cell of the last instruction in the source
	    // expression's TAC.
	    case Print(expr) => {
	      // Translate the source expression.
	      val exprCode = translateExpr(expr, symtab)
	      // Return that code plus a print instruction that prints the
	      // destination cell of the expression code.
	      exprCode :+ PrintInst(exprCode.last.dest.get)
	    }
	    // Assignment statement: translate the source expression to TAC,
	    // then emit a copy instruction with:
	    // - source operand   = destination cell of the last instruction
	    //                      in the source expression's TAC
	    // - destination cell = if the destination variable already has a
	    //                      cell then that cell, otherwise map the
	    //                      variable to a fresh cell and use that
	    //                      (Use the symbol table.)
			// FIXME: implement the Assign case.
	    // case Assign(id, expr) => { }
	  }
	}
	
	/**
	 * Translate a program AST to a sequence of TacInstructions.
	 * 
	 * This is simply the concatenation of the translated statements, in order.
	 * Translations of all statements will share the same symbol table.
	 */
	def translateProgram(prog: Program): Seq[TacInstruction] = {
		var progCode = Vector[TacInstruction]()
		val symtab = Map[String,TacCell]()
		// Accumulate statement code in order, also updating the (mutable)
		// symbol table in sequence.
		for (stmt <- prog.stmts) {
		  progCode = progCode ++ translateStmt(stmt, symtab)
		}
		progCode
	}	

}
