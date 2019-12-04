import scala.collection.mutable
import scala.util.control.Breaks._

val input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,6,23,2,23,6,27,2,6,27,31,2,13,31,35,1,9,35,39,2,10,39,43,1,6,43,47,1,13,47,51,2,6,51,55,2,55,6,59,1,59,5,63,2,9,63,67,1,5,67,71,2,10,71,75,1,6,75,79,1,79,5,83,2,83,10,87,1,9,87,91,1,5,91,95,1,95,6,99,2,10,99,103,1,5,103,107,1,107,6,111,1,5,111,115,2,115,6,119,1,119,6,123,1,123,10,127,1,127,13,131,1,131,2,135,1,135,5,0,99,2,14,0,0"
val inputList = input
  .split(",").map(_.trim.toInt).to[mutable.MutableList]

def run(list: mutable.MutableList[Int]) = {
  breakable {
    for ((f, i) <- list.zipWithIndex; if i % 4 == 0) {
      f match {
        case 99 => break()
        case 1 =>
          val sum = list(list(i + 1)) + list(list(i + 2))
          list(list(i + 3)) = sum
        case 2 =>
          val product = list(list(i + 1)) * list(list(i + 2))
          list(list(i + 3)) = product
        case _ => throw new RuntimeException(s"Unknown opcode: ${i}")
      }
    }
  }

  list.head
}

breakable {
  for (i <- 0 to 99) {
    for (j <- 0 to 99) {
      val updateList = inputList.patch(1, List(i, j), 2)
      val result = run(updateList)

      if (result == 19690720) {
        print(s"Solution: ${100 * i + j}")
        break()
      }
    }
  }
}
