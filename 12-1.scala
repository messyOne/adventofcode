
object Main extends App {
  val inputString = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
  val input = inputString.split("\n")
  val pattern = """<x=(-?\d*), y=(-?\d*), z=(-?\d*)>""".r

  val moons = input.zipWithIndex.foldLeft(List.empty[Moon]) {
    case (moons, data) =>
      val pattern(x, y, z) = data._1

      moons :+ Moon(data._2, Position(x.toInt, y.toInt, z.toInt), Velocity())
  }

  val gravities = moons.combinations(2).foldLeft(List.tabulate(moons.length)(id => Gravity(id))) {
    case (gravities, a :: b :: Nil) =>
      val beforeGravity = gravities.find(_.id == a.id).getOrElse(throw new RuntimeException("Not found"))
      val newGravity = calculateGravity(a, b)

      val updatedGravity = beforeGravity.copy(velocity = beforeGravity.velocity.add(newGravity))
      // TODO update also b. gravity aplyies both

      gravities.filter(_.id != a.id) :+ updatedGravity
  }

  print(gravities)

  def calculateGravity(a: Moon, b: Moon): Velocity = {
    val xDelta = compare(a.position.x, b.position.x)
    val yDelta = compare(a.position.y, b.position.y)
    val zDelta = compare(a.position.z, b.position.z)

    Velocity(xDelta, yDelta, zDelta)
  }

  private def compare(x1: Int, x2: Int): Int = {
    if (x1 > x2) {
      1
    } else if (x1 < x2) {
      -1
    } else 0
  }
}

case class Position(x:Int, y: Int, z: Int)
case class Velocity(x:Int = 0, y: Int = 0, z: Int = 0) {
  def add(velocity: Velocity): Velocity = Velocity(x + velocity.x, y + velocity.y, z + velocity.z)
}
case class Gravity(id: Int, velocity: Velocity = Velocity()) {

}

case class Moon(id: Int, position: Position, velocity: Velocity) {


}
