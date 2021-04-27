package tiny

/**
 * Abstract Syntax Trees
 * 
 * Programs are trees.
 * 
 * An AST is a representation of the abstract structure of a program,
 * with no concrete details of syntax (what symbols or sequence of
 * characters, etc.).  An AST is what a program *is*; the form you
 * write is just a linear notation for this essential structure.
 * 
 * The first stages of a compiler, lexing and parsing, extract this
 * elegant structure from the linear source code string.  Once we have
 * an AST, we never look back.  (OK, a little bit, for usability, but
 * not here.)
 * 
 * ASTs are trees and, as such, they are recursive structures (at least
 * for any interesting language).  Unlike CS 230 trees, where every node
 * has the same basic structure, ASTs have many different types of nodes,
 * one for each syntactic structure in the language.
 */


/** Programs are sequences of statements. */
case class Program(stmts: Seq[Stmt])


/** Statements */
abstract class Stmt
/**
 * An assignment is a statement.
 * It has a destination variable name and a source expression, whose
 * result value is to be computed and stored into this variable.
 */
case class Assign(dest: String, src: Expr) extends Stmt
/**
 * A print is a statement.
 * It has a source expression, whose result value is to be printed.
 */
case class Print(src: Expr) extends Stmt


/** Expressions */
abstract class Expr
/**
 * An input is an expression indicating that a result should be obtained
 * by requesting input from the user.
 */
case class Input() extends Expr
/**
 * A number literal is an expression.  It has a known number value.
 */
case class Num(value: Int) extends Expr
/**
 * An addition is an expression with two subexpressions, left and right,
 * whose result values are to be added.
 */
case class Plus(left: Expr, right: Expr) extends Expr
/**
 * A variable reference is an expression with a name indicating which
 * variable's value should be used.
 */
case class Var(id: String) extends Expr
