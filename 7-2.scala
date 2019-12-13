import IntcodeComputer.State

object Main extends App {
  val values = "3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"
  private val valuesList = values
    .split(",").map(_.trim.toInt).toList

    val winner = (5 to 9).toList.permutations.foldLeft(0) {
    case (throttle, combination) =>
      val (outputA, stateA) = IntcodeComputer.run(State("A", valuesList, List(combination(0), 0))).readOutput()
      val (outputB, stateB) = IntcodeComputer.run(State("B", valuesList, List(combination(1), outputA))).readOutput()
      val (outputC, stateC) = IntcodeComputer.run(State("C", valuesList, List(combination(2), outputB))).readOutput()
      val (outputD, stateD) = IntcodeComputer.run(State("D", valuesList, List(combination(3), outputC))).readOutput()
      val stateE = IntcodeComputer.run(State("E", valuesList, List(combination(4), outputD)))

      @scala.annotation.tailrec
      def traverse(amplifiers: List[State]): Int = {
        if (amplifiers.last.hasStopped) {
          amplifiers.last.readOutput()._1
        } else {
          val (outputE, stateE) = amplifiers.last.readOutput()
          val stateA = IntcodeComputer.continue(amplifiers.head, outputE)

          val newStates = (amplifiers.init :+ stateE).tail.foldLeft(List(stateA)) {
            case (newStates, state) =>
              val (output, newState) = newStates.last.readOutput()

              (newStates.init :+ newState) :+ IntcodeComputer.continue(state, output)
          }

          traverse(newStates)
        }
      }

      val newThrottle = traverse(List(stateA, stateB, stateC, stateD, stateE))

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
