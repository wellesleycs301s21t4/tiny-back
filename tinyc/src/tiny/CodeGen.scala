package tiny

/**
 * Code Generator
 * 
 * This stage translates Intermediate Representation of programs
 * to x86 assembly code.
 * 
 * Since our calculator language includes no functions or control flow,
 * this is fairly straightforward: we emit assembly code for a single
 * procedure.  Each IR instruction expands to a standard pattern of x86
 * instructions.  Every IR location is translated to a location in the
 * stack frame.  We make no effort at important low-level optimizations
 * such as register allocation, smarter instruction selection, liveness
 * analysis, and more. 
 */
object CodeGen {
  /**
   * Translate the IR instructions and add boilerplate.
   */
  def apply(instructions: Seq[TacInstruction]): String = {
		(instructions.foldLeft(CodeGen.prelude + CodeGen.beginMain(maxVar(instructions)))
				((out,inst) => out + CodeGen.inst(inst))
			+ CodeGen.endMain)
  }
  
  /**
   * Find the maximum-numbered cell.
   * We will allocate this many stack slots.
   */
  def maxVar(instructions: Seq[TacInstruction]) = {
    var max = 0
    def count(x: TacOperand) = x match {
      case TacCell(id) if id > max => max = id
      case _ =>
    }
    for (inst <- instructions) {
      inst match {
        case AddInst(dst, src1, src2) => {
          count(dst)
          count(src1)
          count(src2)
        }
        case CopyInst(dst, src) => {
          count(dst)
          count(src)
        }
        case InputInst(dst) => count(dst)
        case PrintInst(src) => count(src)
      }
    }
    max
  }
  
  val MAC = System.getProperty("os.name").equals("Mac OS X")
  val WORD = 8
  val STACK_ALIGN = 16
  /**
   * Generate a label name for the given name.
   */
  def label(lab: String) = {
    if (MAC) {
      "_" + lab
    } else {
      lab
    }
  }
 
  /** Generate the prelude. */
  def prelude = ".globl " + label("main") + "\n\n"
  
  /**
   * Generate the setup code for the main procedure.
   */
  def beginMain(vars: Int) = {
    // Space for: frame pointer, all locals, return address.
    val stackFrame = WORD + vars*WORD + WORD
    // Subtract frame pointer and return address
    val alignedStackSize =  ((stackFrame + STACK_ALIGN) & ~(STACK_ALIGN - 1)) - WORD - WORD
    CodeGen.label("main") + ":\n" +
    "  # SETUP\n" +
    "    pushq %rbp\n" +
    "    movq %rsp,%rbp\n" + 
    "    subq $" + alignedStackSize + ",%rsp\n" +
    "\n  # BODY\n"
  }
  
  /** Generate the cleanup code for the main procedure. */
  def endMain = "\n  # CLEANUP\n    movq $0,%rax\n    leaveq\n    retq\n"
  
  /**
   * Generate the x86 instruction corresponding to the given
   * IR instruction.
   */
  def inst(instruction: TacInstruction) = {
    val ins = instruction match {
      case AddInst(dst, src1, src2) =>
        "    movq " + op(src1) + ",%rax  \n" + "    addq " + op(src2) + ",%rax\n" + "    movq %rax, " + op(dst) + "\n"
      case CopyInst(dst, src) =>
        "    movq " + op(src) + ",%rax  \n" + "    movq %rax," + op(dst) + "\n"
      case InputInst(dst) =>
        "    callq "+ CodeGen.label("tiny_input") + "\n" + "    movq %rax," + op(dst) + "\n"
      case PrintInst(src) =>
        "    movq " + op(src) + ",%rax\n" + "    movq %rax,%rdi\n" + "    callq " + CodeGen.label("tiny_print") + "\n"
    }
    "\n  # " + instruction.toString + "\n" + ins
  }
  
  /**
   * Generate the x86 operand corresponding to the given
   * IR operand.
   */
  def op(operand: TacOperand) = {
    operand match {
      case TacCell(id) => "-" + (WORD*id) + "(%rbp)"
      case TacLiteral(x) => "$" + x
    }
  }
}
