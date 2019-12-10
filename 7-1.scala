object Main extends App {
  val values = "3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"
  private val valuesList = values
    .split(",").map(_.trim.toInt).toList

  val winner = List(0, 1, 2, 3, 4).permutations.foldLeft((0, List.empty[Int])) {
    case ((throttle, combination), c) =>
      val newThrottle = c.foldLeft(0) {
        case (throttle, setting) => IntcodeComputer().exec(valuesList, List(setting, throttle)).head
      }

      if (newThrottle > throttle) {
        (newThrottle, c)
      } else {
        (throttle, combination)
      }
  }

  print(winner)
}

case class IntcodeComputer() {

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

  private def param(l: List[Int], p: Int, modes: Int, arg: Int, forcePositionMode: Boolean = false) = {
    if (mode(modes, arg) == 0 && !forcePositionMode)
      l(l(p + arg)) else
      l(p + arg)
  }

  def exec(values: List[Int], input: List[Int]): List[Int] = iterate(values, input)

  @tailrec
  private def iterate(l: List[Int], input: List[Int], output: List[Int] = Nil, p: Int = 0): List[Int] = {
    val (operator, modes) = splitOp(l(p))

    operator match {
      case 99 => output
      case 1 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, forcePositionMode = true)

        val updatedList = l.patch(op3, List(op1 + op2), 1)
        iterate(updatedList, input, output, p + 4)
      case 2 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, forcePositionMode = true)

        val updatedList = l.patch(op3, List(op1 * op2), 1)

        iterate(updatedList, input, output, p + 4)
      case 3 =>
        val op1 = param(l, p, modes, 1, forcePositionMode = true)

        val updatedList = l.patch(op1, List(input.head), 1)
        iterate(updatedList, input.tail, output, p + 2)
      case 4 =>
        val op1 = param(l, p, modes, 1)

        iterate(l, input, output :+ op1, p + 2)
      case 5 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)

        if (op1 != 0) {
          iterate(l, input, output, op2)
        } else {
          iterate(l, input, output, p + 3)
        }
      case 6 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)

        if (op1 == 0) {
          iterate(l, input, output, op2)
        } else {
          iterate(l, input, output, p + 3)
        }
      case 7 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, forcePositionMode = true)

        val updatedList = if (op1 < op2) {
          l.patch(op3, List(1), 1)
        } else {
          l.patch(op3, List(0), 1)
        }

        iterate(updatedList, input, output, p + 4)
      case 8 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, forcePositionMode = true)

        val updatedList = if (op1 == op2) {
          l.patch(op3, List(1), 1)
        } else {
          l.patch(op3, List(0), 1)
        }

        iterate(updatedList, input, output, p + 4)

      case x => throw new RuntimeException(s"Unknown opcode: $x, Pointer: $p")
    }
  }
}
