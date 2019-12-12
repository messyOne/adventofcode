import IntcodeComputer.State

// TODO not working
object Main extends App {
  val values = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
  private val valuesList = values
    .split(",").map(_.trim.toInt).toList

//  val winner = (5 to 9).toList.permutations.foldLeft(0) {
  val winner = List(List(9,8,7,6,5)).foldLeft(0) {
    case (throttle, combination) =>
      val initializedAmplifiers = combination.foldLeft((List.empty[State])) {
        case (states, phase) =>
          val input = states.lastOption match {
            case Some(state) => state.output
            case None => List(0)
          }

          val newState = IntcodeComputer.run(State(valuesList, List(phase) ::: input))
          states :+ newState
      }

      @scala.annotation.tailrec
      def traverse(amplifiers: List[State]): Int = {
        if (amplifiers.last.hasStopped) {
          amplifiers.last.output.head
        } else {
          val stateA = IntcodeComputer.continue(amplifiers.head, amplifiers.last.output)

          val newStates = amplifiers.tail.foldLeft(List(stateA)) {
            case (newStates, state) =>
              IntcodeComputer.continue(state, newStates.head.output) +: newStates
          }

          traverse(newStates)
        }
      }

      val newThrottle = traverse(initializedAmplifiers)

      if (newThrottle > throttle) {
        newThrottle
      } else {
        throttle
      }
  }

  print(winner)
}

object IntcodeComputer {

  import scala.annotation.tailrec

  case class State(program: List[Int], inputs: List[Int], pointer: Int = 0, output: List[Int] = Nil, isPaused: Boolean = false, hasStopped: Boolean = false) {
    def stop(): State = copy(hasStopped = true)

    def addInput(value: Int): State = copy(inputs = inputs :+ value)
    def addInput(values: List[Int]): State = copy(inputs = inputs ::: values)

    def pause(): State = copy(isPaused = true)

    def resume(): State = copy(isPaused = false)

    def writeOutput(value: Int): State = copy(output = output :+ value)

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

    state.writeOutput(op1).increasePointer(2)
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

