
object Main extends App {
  val inputString = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
  val input = inputString.split("\n")
  val pattern = """<x=(-?\d), y=(-?\d), z=(-?\d)>""".r

  input.foldLeft(List.empty[Moon]) {
    case (moons, positionData) =>
      val  pattern(x, y, z) = positionData

      print(x, y, z)
//      moons :+ Moon(Position(x.toInt, y.toInt, z.toInt), Velocity())
      moons
  }


}

case class Position(x:Int, y: Int, z: Int)
case class Velocity(x:Int = 0, y: Int = 0, z: Int = 0)

case class Moon(position: Position, velocity: Velocity)
