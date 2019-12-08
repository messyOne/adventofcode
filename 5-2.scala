object Main extends App {
  import scala.annotation.tailrec

  val input = 5
  val values = "3,225,1,225,6,6,1100,1,238,225,104,0,1101,86,8,225,1101,82,69,225,101,36,65,224,1001,224,-106,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,102,52,148,224,101,-1144,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,70,45,225,1002,143,48,224,1001,224,-1344,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,69,75,225,1001,18,85,224,1001,224,-154,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,15,59,225,1102,67,42,224,101,-2814,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1101,28,63,225,1101,45,22,225,1101,90,16,225,2,152,92,224,1001,224,-1200,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,45,28,224,1001,224,-73,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1,14,118,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,374,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1007,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,434,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,539,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,569,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,644,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,659,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
//  val values = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\n1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\n999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
  val valuesList = values
    .split(",").map(_.trim.toInt).toList


  def splitOp(op: Int) = {
    if (op.toString.length > 2)
      (op.toString.takeRight(2).toInt, op.toString.dropRight(2).toInt)
    else
      (op, 0)
  }

  def mode(modes: Int, number: Int) = {
    val n = modes.toString.reverse

    if(n.length >= number) {
      n.charAt(number-1).asDigit
    } else 0
  }

  def param(l: List[Int], p: Int, modes: Int, arg: Int, forcePositionMode: Boolean = false) = {

    if (mode(modes, arg) == 0 && !forcePositionMode)
      l(l(p + arg)) else
      l(p + arg)
  }
  @tailrec
  def iterate(l: List[Int], p: Int): List[Int] = {
    val (operator, modes) = splitOp(l(p))

    operator match {
      case 99 => l
      case 1 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, forcePositionMode = true)

        val updatedList = l.patch(op3, List(op1 + op2), 1)
        iterate(updatedList, p + 4)
      case 2 =>
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, forcePositionMode = true)

        val updatedList = l.patch(op3, List(op1 * op2), 1)

        iterate(updatedList, p + 4)
      case 3 =>
        val op1 = param(l, p, modes, 1, forcePositionMode = true)

        val updatedList = l.patch(op1, List(input), 1)
        iterate(updatedList, p + 2)
      case 4 =>
        val op1 = param(l, p, modes, 1)
        print(op1)
        iterate(l, p + 2)
      case 5 =>
        //    Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value
        //    from the second parameter. Otherwise, it does nothing.
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)

        if (op1 != 0) {
          iterate(l, op2)
        } else {
          iterate(l, p + 3)
        }
      case 6 =>
        //    Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value
        //    from the second parameter. Otherwise, it does nothing.
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)

        if (op1 == 0) {
          iterate(l, op2)
        } else {
          iterate(l, p + 3)
        }
      case 7 =>
        //    Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position
        //    given by the third parameter. Otherwise, it stores 0.
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, true)

        val updatedList = if (op1 < op2) {
          l.patch(op3, List(1), 1)
        } else {
          l.patch(op3, List(0), 1)
        }

        iterate(updatedList, p + 4)
      case 8 =>
        //    Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position
        //    given by the third parameter. Otherwise, it stores 0.
        val op1 = param(l, p, modes, 1)
        val op2 = param(l, p, modes, 2)
        val op3 = param(l, p, modes, 3, true)

        val updatedList = if (op1 == op2) {
          l.patch(op3, List(1), 1)
        } else {
          l.patch(op3, List(0), 1)
        }

        iterate(updatedList, p + 4)

      case x => throw new RuntimeException(s"Unknown opcode: $x, Pointer: $p")
    }
  }

  iterate(valuesList, 0)
}
