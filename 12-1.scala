import Main.gravities

object Main extends App {
  val inputString = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
  val input = inputString.split("\n")
  val pattern = """<x=(-?\d*), y=(-?\d*), z=(-?\d*)>""".r

  val moons = input.zipWithIndex.foldLeft(List.empty[Moon]) {
    case (moons, data) =>
      val pattern(x, y, z) = data._1

      moons :+ Moon(data._2, Vector(x.toInt, y.toInt, z.toInt), Vector())
  }

  val gravities = moons.combinations(2).foldLeft(List.tabulate(moons.length)(id => Gravity(id))) {
    case (gravities, a :: b :: Nil) =>
      def updateGravity(a: Moon, b: Moon): Gravity = {
        val beforeGravityA = gravities.find(_.id == a.id).getOrElse(throw new RuntimeException("Not found"))
        val newGravityA = calculateGravity(a, b)

        beforeGravityA.copy(velocity = beforeGravityA.velocity.add(newGravityA))
      }

      gravities.filter(g => !List(a.id, b.id).contains(g.id)) :+ updateGravity(a, b) :+ updateGravity(b, a)
  }

  // run in loop 10 times

  val moonsUpdated = gravities.foldLeft(moons) {
    case (moons, gravity) =>
      val moon = moons.find(_.id == gravity.id).getOrElse(throw new RuntimeException("Not found"))
      moons.filter(_.id != moon.id) :+ moon.step(gravity)
  }

  moonsUpdated.foreach(m => print(m.toString))

  def calculateGravity(a: Moon, b: Moon): Vector = {
    val xDelta = compare(a.position.x, b.position.x)
    val yDelta = compare(a.position.y, b.position.y)
    val zDelta = compare(a.position.z, b.position.z)

    Vector(xDelta, yDelta, zDelta)
  }

  private def compare(x1: Int, x2: Int): Int = {
    if (x1 > x2) {
      1
    } else if (x1 < x2) {
      -1
    } else 0
  }
}

case class Vector(x:Int = 0, y: Int = 0, z: Int = 0) {
  def add(velocity: Vector): Vector = Vector(x + velocity.x, y + velocity.y, z + velocity.z)

  override def toString: String = s"<x=${x}, y=${y}, z=${z}>"
}
case class Gravity(id: Int, velocity: Vector = Vector()) {

}

case class Moon(id: Int, position: Vector, velocity: Vector) {
  def step(gravity: Gravity): Moon = {
    val newVelocity = velocity.add(gravity.velocity)
    copy(position = position.add(newVelocity), velocity = newVelocity)
  }

  override def toString: String = {
    s"Id: ${id}\nPos: ${position}, Vel: ${velocity}\n"
  }
}
