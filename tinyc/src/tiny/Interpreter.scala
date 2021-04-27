package tiny
import java.util.Scanner
import java.io.File
import scala.collection.mutable

/**
  * A Tiny Calculator Language Interpreter
  * 
  * Our interpreter runs Tiny programs directly. It shares only
  * parsing with the compiler.
  */
object Interpreter {

  /** Evaluate an expression in the given dynamic environment. */
  def evalExpr(expr: Expr, env: mutable.Map[String, Int]): Int = {
    expr match {
      // Input expression: prompt user for input.
      case Input() => scala.io.StdIn.readLine("Input: ").toInt
      // A number is already a value.
      case Num(num) => num
      // A plus expression Scala-adds the values resulting from
      // evaluating both subexpressions.
      case Plus(left, right) => evalExpr(left, env) + evalExpr(right, env)
      // A variable reference looks up the current value for the
      // variable in the environment. If the variable is not defined
      // in the environment, this is an error.
      case Var(id) => env.get(id) match {
        case Some(num) => num
        case None => throw new UndefinedVariableError(id)
      }
    }
  }

  /** Evaluate a statement in (and possibly mutating) the given dynamic
    * environment. */
  def evalStmt(stmt: Stmt, env: mutable.Map[String, Int]): Unit = {
    stmt match {
      // An assignment statement evaluates the expression in the
      // current dynamic environment, then updates the value of the
      // target variable in the dynamic environment.
      case Assign(dest, expr) => env.put(dest, evalExpr(expr, env))
      // A print statement evalues the expression in the current
      // dynamic environment, then prints the result.
      case Print(expr) => println("Output: " + evalExpr(expr, env))
    }
  }

  /** Evaluate a program. */
  def evalProgram(program: Program): Unit = {
    // Initially, the dynamic environment is empty.
    val env : mutable.HashMap[String, Int] = mutable.HashMap()
    // Statements are executed in order, using (and possibly mutating)
    // the dynamic environment.
    for (stmt <- program.stmts) {
      evalStmt(stmt, env)
    }
  }

  /** Main interpreter entrypoint: parse the given source file into an
    * AST, then evaluate it. */
  def main(args: Array[String]): Unit = {
    // 0. LOAD the source code program from the file given as a
    //    command line argument.
	val source = new Scanner(new File(args(0)))

	// 1. PARSE the source code program to produce an AST.
	val ast = Parse(source)

    // 2. EVALUATE the AST.
    evalProgram(ast)
  }
}

/**
  *  Error to be raised at run time when attempting to use a variable
  *  that has not yet been defined.
  */
class UndefinedVariableError(id: String) extends Exception("TINY: Variable " + id + " is not defined.")

