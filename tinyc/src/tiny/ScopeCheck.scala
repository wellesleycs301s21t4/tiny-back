package tiny

/**
  * Scope Checking
  *
  * The compiler checks to make sure all uses of a variable occur only
  * after its definition.  This is nearly trivial for our tiny language
  * and it is the only semantic (AST) analysis we do in this tiny
  * compiler, so we have wrapped it up here with translation to TAC.
  *
  * Just as the parser was composed of recursive functions that built up
  * the recursive AST structure, functions that inspect or consume the AST
  * follow similar recursive structure.
  */
object ScopeCheck {
  def apply(ast: Program): Set[String] = {
    checkProgram(ast)
  }

  /**
    * Check that all used variables are in the defined set.
    * Throw a ScopeError if not.
    */
  def checkExpr(expr: Expr, defined: Set[String]): Unit = {
    expr match {
      // Input and Num expressions never use variables. Do nothing.
      case Input() =>
      case Num(x) =>
      // Plus expressions may use variables in their left or right
      // subexpressions. Check those.
      case Plus(left, right) => {
        checkExpr(left, defined)
        checkExpr(right, defined)
      }
      // Check that this variable is in the defined set.
      case Var(id) => {
        if (!defined.contains(id)) {
          throw new ScopeError(id)
        }
      }
    }
  }

  /**
    * Check that all used variables are in defined the set. Add any
    * assigned variable to the defined set.
    */
  def checkStmt(stmt: Stmt, defined: Set[String]): Set[String] = {
    stmt match {
      // Print statement: check variables used in the expression.
      case Print(expr) => {
        checkExpr(expr, defined)
        defined
      }
      // Assignment statement: check variables used in the expression,
      // then define the variable assigned.
      case Assign(id, expr) => {
        checkExpr(expr, defined)
        defined + id
      }
    }
  }

  /**
    * Accumulate a set of defined variables and check that all used
    * variables are in the set.
    */
  def checkProgram(prog: Program): Set[String] = {
    var defined = Set[String]()
    for (stmt <- prog.stmts) {
      defined = checkStmt(stmt, defined)
    }
    defined
  }

  /** Scope errors, when variables are used before definitions. */
  class ScopeError(id: String)
    extends CompilerError("Variable " + id + " used before definition.")

}
