
object Main extends App with NBody {
  val inputString = "<x=14, y=2, z=8>\n<x=7, y=4, z=10>\n<x=1, y=17, z=16>\n<x=-4, y=-1, z=1>"
  val input = inputString.split("\n")
  val pattern = """<x=(-?\d*), y=(-?\d*), z=(-?\d*)>""".r

  val moons = input.zipWithIndex.foldLeft(List.empty[Moon]) {
    case (moons, data) =>
      val pattern(x, y, z) = data._1

      moons :+ Moon(data._2, Vector(x.toInt, y.toInt, z.toInt), Vector())
  }

  val totalEnergy = run(moons).map(_.energy()).sum

  print(s"Total energy: ${totalEnergy}")
}

trait NBody {
  def run(moons: List[Moon], count: Int = 1): List[Moon] = {
    val gravities = moons.combinations(2).foldLeft(List.tabulate(moons.length)(id => Gravity(id))) {
      case (gravities, a :: b :: Nil) =>
        def updateGravity(a: Moon, b: Moon): Gravity = {
          val beforeGravityA = gravities.find(_.id == a.id).getOrElse(throw new RuntimeException("Not found"))
          val gravity = calculateGravity(a, b)

          beforeGravityA.copy(velocity = beforeGravityA.velocity.add(gravity))
        }

        val aUpdated = updateGravity(a, b)
        val bUpdated = updateGravity(b, a)
        gravities.filter(g => g.id != a.id && g.id != b.id) :+ aUpdated :+ bUpdated
    }


    val updatedMoons = gravities.foldLeft(moons) {
      case (moons, gravity) =>
        val moon = moons.find(_.id == gravity.id).getOrElse(throw new RuntimeException("Not found"))
        moons.filter(_.id != moon.id) :+ moon.step(gravity)
    }

//    Debug
//    print(s"After ${count} steps: \n")
//    updatedMoons.foreach(m => print(m.toString))

    if (count >= 1000) {
      updatedMoons
    } else {
      run(updatedMoons, count + 1)
    }
  }

  def calculateGravity(a: Moon, b: Moon): Vector = {
    val xDelta = compare(a.position.x, b.position.x)
    val yDelta = compare(a.position.y, b.position.y)
    val zDelta = compare(a.position.z, b.position.z)

    Vector(xDelta, yDelta, zDelta)
  }

  private def compare(x1: Int, x2: Int): Int = {
    if (x1 > x2) {
      -1
    } else if (x1 < x2) {
      1
    } else 0
  }
}

case class Vector(x:Int = 0, y: Int = 0, z: Int = 0) {
  def sum(): Int = x.abs + y.abs + z.abs

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

  def energy(): Int = position.sum() * velocity.sum()

  override def toString: String = {
    s"Id: ${id}\nPos: ${position}, Vel: ${velocity}\n"
  }
}
