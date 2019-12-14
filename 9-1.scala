import IntcodeComputer.State

object Main extends App {
  def parseValues(values: String) = values.split(",").map(_.trim.toLong).toList

  assert(IntcodeComputer.run(State("A", parseValues("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))).output == List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
  assert(IntcodeComputer.run(State("B", parseValues("1102,34915192,34915192,7,4,7,99,0"))).output.head.toString.length == 16)
  assert(IntcodeComputer.run(State("C", parseValues("104,1125899906842624,99"))).output.head == 1125899906842624L)

  val d = List[Long](0, 1, 2, 3, 4).permutations.foldLeft((0L, List.empty[Long])) {
    case ((throttle, combination), c) =>
      val newThrottle = c.foldLeft(0L) {
        case (throttle, setting) =>
          val values = "3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"
          IntcodeComputer.run(State("D", parseValues(values), List(setting, throttle))).output.head
      }

      if (newThrottle > throttle) {
        (newThrottle, c)
      } else {
        (throttle, combination)
      }
  }
  assert(d._1 == 24625)

  val values = "3,225,1,225,6,6,1100,1,238,225,104,0,1101,86,8,225,1101,82,69,225,101,36,65,224,1001,224,-106,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,102,52,148,224,101,-1144,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,70,45,225,1002,143,48,224,1001,224,-1344,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,69,75,225,1001,18,85,224,1001,224,-154,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,15,59,225,1102,67,42,224,101,-2814,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1101,28,63,225,1101,45,22,225,1101,90,16,225,2,152,92,224,1001,224,-1200,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,45,28,224,1001,224,-73,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1,14,118,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,374,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1007,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,434,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,539,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,569,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,644,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,659,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
  assert(IntcodeComputer.run(State("E", parseValues(values), List(5))).output.head == 14195011)
  assert(IntcodeComputer.run(State("F", parseValues(values), List(1))).output.last == 10987514)

  private val value = "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,0,33,1003,1101,0,23,1002,1102,1,557,1022,1102,1,24,1010,1102,1,22,1014,1101,470,0,1027,1102,38,1,1001,1102,1,21,1012,1102,1,1,1021,1101,0,26,1018,1101,0,827,1024,1101,690,0,1029,1101,0,473,1026,1102,1,27,1015,1101,695,0,1028,1101,822,0,1025,1102,1,35,1019,1102,1,30,1000,1101,0,39,1013,1101,25,0,1016,1101,28,0,1006,1102,1,36,1004,1101,34,0,1011,1101,31,0,1017,1101,0,0,1020,1101,29,0,1009,1102,1,554,1023,1102,32,1,1007,1101,37,0,1008,1101,20,0,1005,109,5,2101,0,0,63,1008,63,20,63,1005,63,203,4,187,1106,0,207,1001,64,1,64,1002,64,2,64,109,-4,2107,21,4,63,1005,63,227,1001,64,1,64,1105,1,229,4,213,1002,64,2,64,109,4,2108,37,3,63,1005,63,251,4,235,1001,64,1,64,1106,0,251,1002,64,2,64,109,12,21101,40,0,-5,1008,1012,38,63,1005,63,275,1001,64,1,64,1105,1,277,4,257,1002,64,2,64,109,-14,21108,41,41,10,1005,1013,299,4,283,1001,64,1,64,1105,1,299,1002,64,2,64,109,5,1202,-4,1,63,1008,63,36,63,1005,63,321,4,305,1106,0,325,1001,64,1,64,1002,64,2,64,109,-3,2108,38,-1,63,1005,63,345,1001,64,1,64,1106,0,347,4,331,1002,64,2,64,109,-8,1201,4,0,63,1008,63,40,63,1005,63,367,1105,1,373,4,353,1001,64,1,64,1002,64,2,64,109,20,1205,4,391,4,379,1001,64,1,64,1106,0,391,1002,64,2,64,109,5,1205,-2,407,1001,64,1,64,1106,0,409,4,397,1002,64,2,64,109,-15,2102,1,-3,63,1008,63,36,63,1005,63,431,4,415,1106,0,435,1001,64,1,64,1002,64,2,64,109,-6,1202,6,1,63,1008,63,31,63,1005,63,459,1001,64,1,64,1105,1,461,4,441,1002,64,2,64,109,28,2106,0,-2,1105,1,479,4,467,1001,64,1,64,1002,64,2,64,109,-14,21107,42,41,-4,1005,1011,499,1001,64,1,64,1106,0,501,4,485,1002,64,2,64,109,8,1206,-3,515,4,507,1105,1,519,1001,64,1,64,1002,64,2,64,109,-29,2101,0,6,63,1008,63,33,63,1005,63,539,1105,1,545,4,525,1001,64,1,64,1002,64,2,64,109,30,2105,1,-1,1106,0,563,4,551,1001,64,1,64,1002,64,2,64,109,5,1206,-8,579,1001,64,1,64,1106,0,581,4,569,1002,64,2,64,109,-31,1201,3,0,63,1008,63,38,63,1005,63,607,4,587,1001,64,1,64,1106,0,607,1002,64,2,64,109,11,21101,43,0,4,1008,1013,43,63,1005,63,633,4,613,1001,64,1,64,1106,0,633,1002,64,2,64,109,-10,2107,22,3,63,1005,63,651,4,639,1106,0,655,1001,64,1,64,1002,64,2,64,109,26,21102,44,1,-8,1008,1017,44,63,1005,63,681,4,661,1001,64,1,64,1105,1,681,1002,64,2,64,109,-3,2106,0,6,4,687,1105,1,699,1001,64,1,64,1002,64,2,64,109,-3,21108,45,43,0,1005,1019,715,1105,1,721,4,705,1001,64,1,64,1002,64,2,64,109,-25,1207,9,32,63,1005,63,737,1105,1,743,4,727,1001,64,1,64,1002,64,2,64,109,18,21107,46,47,3,1005,1015,761,4,749,1106,0,765,1001,64,1,64,1002,64,2,64,109,-3,2102,1,-3,63,1008,63,31,63,1005,63,789,1001,64,1,64,1105,1,791,4,771,1002,64,2,64,109,-5,1208,-4,30,63,1005,63,813,4,797,1001,64,1,64,1105,1,813,1002,64,2,64,109,28,2105,1,-8,4,819,1106,0,831,1001,64,1,64,1002,64,2,64,109,-30,1207,0,24,63,1005,63,853,4,837,1001,64,1,64,1106,0,853,1002,64,2,64,109,16,21102,47,1,-7,1008,1011,45,63,1005,63,873,1105,1,879,4,859,1001,64,1,64,1002,64,2,64,109,-21,1208,5,26,63,1005,63,899,1001,64,1,64,1105,1,901,4,885,4,64,99,21102,27,1,1,21102,915,1,0,1106,0,922,21201,1,69417,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1106,0,922,21201,1,0,-1,21201,-2,-3,1,21101,0,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,22102,1,-2,-2,109,-3,2106,0,0"
  print(IntcodeComputer.run(State("G", parseValues(value), List(1))).output)
}

object IntcodeComputer {

  import scala.annotation.tailrec

  case class State(label: String, program: List[Long], inputs: List[Long] = Nil, pointer: Long = 0, output: List[Long] = Nil, relativeBase: Long = 0, isPaused: Boolean = false, hasStopped: Boolean = false) {
    def stop(): State = copy(hasStopped = true)

    def addInput(value: Long): State = copy(inputs = inputs :+ value)

    def addInput(values: List[Long]): State = copy(inputs = inputs ::: values)

    def pause(): State = copy(isPaused = true)

    def resume(): State = copy(isPaused = false)

    def addOutput(value: Long): State = copy(output = output :+ value)

    def readOutput(): (Long, State) = (output.head, copy(output = output.tail))

    def patchProgram(position: Long, value: Long): State = {
      copy(program = program.patch(position.toInt, List(value), 1))
    }

    def readInput(): (Option[Long], State) = {
      inputs.headOption match {
        case input: Some[Long] => (input, copy(inputs = inputs.tail))
        case None => (None, this)
      }
    }

    def updateProgram(program: List[Long]): State = copy(program = program)

    def increasePointer(by: Long): State = copy(pointer = pointer + by)

    def pointer(pointer: Long): State = copy(pointer = pointer)

    def step: Long = program(pointer.toInt)

    def arg(number: Long): Long = program((pointer + number).toInt)
  }

  private def splitOp(op: Long) = {
    if (op.toString.length > 2)
      (op.toString.takeRight(2).toLong, op.toString.dropRight(2).toLong)
    else
      (op, 0L)
  }

  private def mode(modes: Long, number: Long) = {
    val n = modes.toString.reverse

    if (n.length >= number) {
      n.charAt(number.toInt - 1).asDigit.toLong
    } else 0L
  }

  private def param(state: State, modes: Long, arg: Long, forcePositionMode: Boolean = false) = {
    if (mode(modes, arg) == 2) {
      state.program((state.arg(arg) + state.relativeBase).toInt)
    } else if (mode(modes, arg) == 0 && !forcePositionMode) {
      state.program(state.arg(arg).toInt)
    } else {
      state.arg(arg)
    }
  }

  def run(state: State): State = iterate(state.copy(program = state.program ::: List.fill(10000)(0L)))

  def continue(state: State, input: Long): State = run(state.addInput(input).resume())

  def continue(state: State, inputs: List[Long]): State = run(state.addInput(inputs).resume())


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
      case 9 =>
        iterate(relativeBaseOffset(state, modes))
      case x => throw new RuntimeException(s"Unknown opcode: $x, Pointer: ${state.pointer}")
    }
  }

  private def equal(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    val newState = if (op1 == op2) {
      state.patchProgram(op3, 1)
    } else {
      state.patchProgram(op3, 0)
    }

    newState.increasePointer(4)
  }

  private def lessThan(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    val newState = if (op1 < op2) {
      state.patchProgram(op3, 1)
    } else {
      state.patchProgram(op3, 0)

    }

    newState.increasePointer(4)
  }

  private def jumpIfFalse(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)

    if (op1 == 0) {
      state.pointer(op2)
    } else {
      state.increasePointer(3)
    }
  }

  private def jumpIfTrue(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)

    if (op1 != 0) {
      state.pointer(op2)
    } else {
      state.increasePointer(3)
    }
  }

  private def output(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)

    state.addOutput(op1).increasePointer(2)
  }

  private def input(state: State, modes: Long) = {
    val op1 = param(state, modes, 1, forcePositionMode = true)

    val (input, newState) = state.readInput()

    input match {
      case Some(input) =>
        newState.patchProgram(op1, input).increasePointer(2)
      case None => state.pause()
    }
  }

  private def multiplication(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    state.patchProgram(op3, op1 * op2).increasePointer(4)
  }

  private def addition(state: State, modes: Long) = {
    val op1 = param(state, modes, 1)
    val op2 = param(state, modes, 2)
    val op3 = param(state, modes, 3, forcePositionMode = true)

    state.patchProgram(op3, op1 + op2).increasePointer(4)
  }

  private def relativeBaseOffset(state: State, modes: Long): State = {
    val op1 = param(state, modes, 1)

    state.copy(relativeBase = state.relativeBase + op1).increasePointer(2)
  }
}
