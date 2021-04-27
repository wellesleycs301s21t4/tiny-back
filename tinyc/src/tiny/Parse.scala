package tiny

import java.util.Scanner

/**
  * Lexical and Syntactic Analysis
  *
  * This stages takes source code as a string (abstracted here by a Scanner)
  * and extracts an Abstract Syntax Tree, the abstract structure of the
  * program that is encoding by the low-level string of characters.
  *
  * Compilers typically split this into two stages:
  * 1. Lexical Analysis (a.k.a. lexing a.k.a. scanning)
  * transforms an unstructured string of characters into a stream of
  * lexemes or tokens -- the notational atoms of the language.
  * 2. Syntax Analysis (a.k.a. parsing)
  * transforms a stream of tokens into a tree capturing the abstract
  * structure of the program: an Abstract Syntax Tree.
  *
  * Here, with a tiny compiler for a tiny language, we will sort of combine
  * lexing and parsing in one stage.  The Scanner will do lexing for us,
  * but it does not "know" the language.  We ask it for specific patterns
  * of characters as we go.  We will see a cleaner way to define all this
  * in the next couple weeks of the course.
  *
  * The parser must consume tokens in a linear order, even though it will
  * produce a structure that is not linear.  The code of the parser will
  * be recursive, reflecting the recursive structure of the language syntax.
  * We implement a simple Recursive Descent parser, which has one function
  * to parse each kind nonterminal in the grammar (Program, Stmt, Expr).
  */
object Parse {
  def apply(source: Scanner) = Parse.parseProgram(source)

  /**
    * Parse an expression from a Scanner and produce some kind of Expr AST node.
    */
  def parseExpr(source: Scanner): Expr = {
    if (source.hasNext("\\(")) { // test for token
      source.next("\\(") // consume token
      // If the next token is (, the expression is either ( _ ) or ( _ + _ ).
      // Either way, the next thing in the stream should be another
      // expression, so parse it!
      val subexpr = parseExpr(source)
      // Now, we need to distinguish between ( _ ) and (_ + _ ).
      val result =
        if (source.hasNext("\\)")) {
          // If there is a closing ) next, then this is just the subexpression
          // wrapped in an extra set of unnecessary parentheses.  The result
          // is the subexpression AST.
          subexpr
        } else if (source.hasNext("\\+")) {
          source.next("\\+")
          // If there is a + next, then this is ( subexpr + _ ).
          // Parse that second expression and pair with the first
          // for the resulting Plus-expression AST node.
          Plus(subexpr, parseExpr(source))
        } else {
          // If we see any other token next, it is a syntax error.
          throw new SyntaxError(source)
        }
      // Regardless of whether it was ( _ ) or ( _ + _ ), it ends with ).
      // If not, it's a syntax error.
      if (source.hasNext("\\)")) source.next("\\)")
      else throw new SyntaxError(source)
      // Return the result AST.
      result
    } else if (source.hasNextInt) {
      // If the next token is an integer literal, the expression is an
      // integer literal.
      Num(source.nextInt())
    } else if (source.hasNext("input")) {
      // If the next token is "input", the next expression is an input
      // expression.
      source.next("input")
      Input()
    } else if (source.hasNext("[a-zA-Z]+")) {
      // If the next token is some string of word characters, it is
      // a variable reference expression.
      Var(source.next("[a-zA-Z]+"))
    } else {
      // If there is any other token next, it is a syntax error.
      throw new SyntaxError(source)
    }
  }

  /**
    * Parse a statement from a Scanner and produce some kind of Stmt AST node.
    */
  def parseStmt(source: Scanner): Stmt = {
    val result =
      if (source.hasNext("print")) {
        source.next("print")
        // If the statement starts with "print", it is a print statement
        // with a subexpression that will give the value to print.
        // This makes a print statement.
        Print(parseExpr(source))
      } else if (source.hasNext("[a-zA-Z]+")) {
        // This pattern indicates "a string of at least one letter."
        val id = source.next("[a-zA-Z]+")
        // If the statement starts with some other letter-first word, it is
        // a variable assignment statement where this first word is the
        // name of the variable and must be followed by an = token.
        if (source.hasNext("=")) source.next("=")
        else throw new SyntaxError(source)
        // Finally, after the = token, there is an expression, whose
        // value is to be assigned to the given variable.
        // Altogether, these make an assignment statement.
        Assign(id, parseExpr(source))
      } else {
        // There are no other tokens that could start a legal statement.
        throw new SyntaxError(source)
      }
    // All statements must be terminated with ;
    if (source.hasNext(";")) source.next(";")
    else throw new SyntaxError(source)
    result
  }

  /**
    * Parse a program from a Scanner and produce an AST.
    */
  def parseProgram(source: Scanner): Program = {
    // A Program is a sequence of statements.
    var prog = Vector[Stmt]()
    // While there are more tokens available, parse another statement
    // and append it to the sequence.
    while (source.hasNext()) {
      prog = prog :+ parseStmt(source)
    }
    Program(prog)
  }

  // Parse error reporting
  class SyntaxError(source: Scanner) extends CompilerError("at symbol " + source.next())

}
