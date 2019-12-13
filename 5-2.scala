import IntcodeComputer.State

object Main extends App {

  val input = 5
  val values = "3,225,1,225,6,6,1100,1,238,225,104,0,1101,86,8,225,1101,82,69,225,101,36,65,224,1001,224,-106,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,102,52,148,224,101,-1144,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,70,45,225,1002,143,48,224,1001,224,-1344,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,69,75,225,1001,18,85,224,1001,224,-154,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,15,59,225,1102,67,42,224,101,-2814,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1101,28,63,225,1101,45,22,225,1101,90,16,225,2,152,92,224,1001,224,-1200,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,45,28,224,1001,224,-73,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1,14,118,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,374,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1007,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,434,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,539,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,569,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,644,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,659,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
  //  val values = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\n1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\n999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
  val valuesList = values
    .split(",").map(_.trim.toInt).toList

  print(IntcodeComputer.run(State("Computer", valuesList, List(input))).readOutput()._1)
}

object IntcodeComputer {

  import scala.annotation.tailrec

  case class State(label: String, program: List[Int], inputs: List[Int], pointer: Int = 0, output: List[Int] = Nil, isPaused: Boolean = false, hasStopped: Boolean = false) {
    def stop(): State = copy(hasStopped = true)

    def addInput(value: Int): State = copy(inputs = inputs :+ value)

    def addInput(values: List[Int]): State = copy(inputs = inputs ::: values)

    def pause(): State = copy(isPaused = true)

    def resume(): State = copy(isPaused = false)

    def addOutput(value: Int): State = copy(output = output :+ value)

    def readOutput(): (Int, State) = (output.head, copy(output = output.tail))

    def readInput(): (Option[Int], State) = {
      inputs.headOption match {
        case input: Some[Int] => (input, copy(inputs = inputs.tail))
        case None => (None, this)
      }
    }

    def updateProgram(program: List[Int]): State = copy(program = program)

    def increasePointer(by: Int): State = copy(pointer = pointer + by)

    def pointer(pointer: Int): State = copy(pointer = pointer)

    def step: Int = program(pointer)

    def arg(number: Int): Int = program(pointer + number)
  }

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

  def continue(state: State, input: Int): State = run(state.addInput(input).resume())

  def continue(state: State, inputs: List[Int]): State = run(state.addInput(inputs).resume())

  @tailrec
  private def iterate(state: State): State = {
    val (operator, modes) = splitOp(state.step)

    operator match {
      case 99 => state.stop()
      case 1 =>
        iterate(addition(state, modes))
      case 2 =>
        iterate(multiplication(state, modes))
      case 3 =>
        val newState = input(state, modes)

        if (newState.isPaused) {
          newState
        } else {
          iterate(newState)
        }
      case 4 =>
        iterate(output(state, modes))
      case 5 =>
        iterate(jumpIfTrue(state, modes))
      case 6 =>
        iterate(jumpIfFalse(state, modes))
      case 7 =>
        iterate(lessThan(state, modes))
      case 8 =>
        iterate(equal(state, modes))
      case x => throw new RuntimeException(s"Unknown opcode: $x, Pointer: ${state.pointer}")
    }
  }

  private def equal(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    val updatedProgram = if (op1 == op2) {
      state.program.patch(op3, List(1), 1)
    } else {
      state.program.patch(op3, List(0), 1)
    }

    state.updateProgram(updatedProgram).increasePointer(4)
  }

  private def lessThan(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    val updatedProgram = if (op1 < op2) {
      state.program.patch(op3, List(1), 1)
    } else {
      state.program.patch(op3, List(0), 1)
    }

    state.updateProgram(updatedProgram).increasePointer(4)
  }

  private def jumpIfFalse(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)

    if (op1 == 0) {
      state.pointer(op2)
    } else {
      state.increasePointer(3)
    }
  }

  private def jumpIfTrue(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)

    if (op1 != 0) {
      state.pointer(op2)
    } else {
      state.increasePointer(3)
    }
  }

  private def output(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)

    state.addOutput(op1).increasePointer(2)
  }

  private def input(state: State, modes: Int) = {
    val op1 = param(state, modes, 1, forcePositionMode = true)

    val (input, newState) = state.readInput()

    input match {
      case Some(input) =>
        val updatedProgram = newState.program.patch(op1, List(input), 1)
        newState.updateProgram(updatedProgram).increasePointer(2)
      case None => state.pause()
    }
  }

  private def multiplication(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    val updatedProgram = state.program.patch(op3, List(op1 * op2), 1)
    state.updateProgram(updatedProgram).increasePointer(4)
  }

  private def addition(state: State, modes: Int) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    val updatedProgram = state.program.patch(op3, List(op1 + op2), 1)
    state.updateProgram(updatedProgram).increasePointer(4)
  }
}
