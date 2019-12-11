import IntcodeComputer.State

object Main extends App {
  val values = "3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"
  private val valuesList = values
    .split(",").map(_.trim.toInt).toList

  val winner = List(0, 1, 2, 3, 4).permutations.foldLeft((0, List.empty[Int])) {
    case ((throttle, combination), c) =>
      val newThrottle = c.foldLeft(0) {
        case (throttle, setting) =>
          new IntcodeComputer().run(State(valuesList, List(setting, throttle))).output.head
      }

      if (newThrottle > throttle) {
        (newThrottle, c)
      } else {
        (throttle, combination)
      }
  }

  print(winner._1)
}

object IntcodeComputer {
  case class State(program: List[Int], input: List[Int], pointer: Int = 0, output: List[Int] = Nil) {
    def writeOutput(value: Int): State = copy(output = output :+ value)
    def readInput(): (List[Int], State) = (List(input.head), copy(input = input.tail))
    def updateProgram(program: List[Int]): State = copy(program = program)
    def increasePointer(by: Int): State = copy(pointer = pointer + by)
    def pointer(pointer: Int): State = copy(pointer = pointer)
    def step: Int = program(pointer)
    def arg(number: Int) : Int = program(pointer + number)
  }
}

class IntcodeComputer() {
  import scala.annotation.tailrec

  private def splitOp(op: Int) = {
    if (op.toString.length > 2)
      (op.toString.takeRight(2).toInt, op.toString.dropRight(2).toInt)
    else
      (op, 0)
  }

  private def mode(modes: Int, number: Int) = {
    val n = modes.toString.reverse

    if (n.length >= number) {
      n.charAt(number - 1).asDigit
    } else 0
  }

  private def param(state: State, modes: Int, arg: Int, forcePositionMode: Boolean = false) = {
    if (mode(modes, arg) == 0 && !forcePositionMode) {
      state.program(state.arg(arg))
    } else {
      state.arg(arg)
    }
  }

  def run(state: State): State = iterate(state)

  @tailrec
  private def iterate(state: State): State = {
    val (operator, modes) = splitOp(state.step)

    operator match {
      case 99 => state
      case 1 =>
        val op1 = param(state, modes, 1)
        val op2 = param(state, modes, 2)
        val op3 = param(state, modes, 3, forcePositionMode = true)
        
        val updatedProgram = state.program.patch(op3, List(op1 + op2), 1)
        iterate(state.updateProgram(updatedProgram).increasePointer(4))
      case 2 =>
        val op1 = param(state, modes, 1)
        val op2 = param(state, modes, 2)
        val op3 = param(state, modes, 3, forcePositionMode = true)

        val updatedProgram = state.program.patch(op3, List(op1 * op2), 1)
        iterate(state.updateProgram(updatedProgram).increasePointer(4))
      case 3 =>
        val op1 = param(state, modes, 1, forcePositionMode = true)

        val (input, newState) = state.readInput()
        val updatedProgram = newState.program.patch(op1, input, 1)
        iterate(newState.updateProgram(updatedProgram).increasePointer(2))
      case 4 =>
        val op1 = param(state, modes, 1)

        iterate(state.writeOutput(op1).increasePointer(2))
      case 5 =>
        val op1 = param(state, modes, 1)
        val op2 = param(state, modes, 2)

        if (op1 != 0) {
          iterate(state.pointer(op2))
        } else {
          iterate(state.increasePointer(3))
        }
      case 6 =>
        val op1 = param(state, modes, 1)
        val op2 = param(state, modes, 2)

        if (op1 == 0) {
          iterate(state.pointer(op2))
        } else {
          iterate(state.increasePointer(3))
        }
      case 7 =>
        val op1 = param(state, modes, 1)
        val op2 = param(state, modes, 2)
        val op3 = param(state, modes, 3, forcePositionMode = true)

        val updatedProgram = if (op1 < op2) {
          state.program.patch(op3, List(1), 1)
        } else {
          state.program.patch(op3, List(0), 1)
        }

        iterate(state.updateProgram(updatedProgram).increasePointer(4))
      case 8 =>
        val op1 = param(state, modes, 1)
        val op2 = param(state, modes, 2)
        val op3 = param(state, modes, 3, forcePositionMode = true)

        val updatedProgram = if (op1 == op2) {
          state.program.patch(op3, List(1), 1)
        } else {
          state.program.patch(op3, List(0), 1)
        }

        iterate(state.updateProgram(updatedProgram).increasePointer(4))

      case x => throw new RuntimeException(s"Unknown opcode: $x, Pointer: ${state.pointer}")
    }
  }
}
