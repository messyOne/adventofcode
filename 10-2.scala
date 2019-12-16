
object Main extends App {
  val input: Seq[List[Char]] = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##".split("\n").toList.map(_.toList)
  case class Asteroid(position: (Int, Int), distance: Double, arc: Double, group: Int = -1)

  def analyze(position: (Int, Int)) = {
    input.zipWithIndex.foldLeft(List.empty[Asteroid]) {
      case (asteroids, (row, y)) =>
        row.zipWithIndex.filter(cx => cx._1 == '#').map(_._2).foldLeft(asteroids) {
          case (asteroids, x) if position == (y, x) => asteroids
          case (asteroids, x) =>
            val arc = Math.atan2(position._1-y, position._2-x)
            val distance = Math.sqrt((position._1-x)*(position._1-x) + (position._2-y)*(position._2-y))

            asteroids :+ Asteroid((y, x), distance, arc)
        }
    }
  }

  val result = analyze(13, 11).sortBy(a => (a.arc, a.distance)).partition(_.arc >= Math.PI/2)

  @scala.annotation.tailrec
  def traverse(asteroids: List[Asteroid], count: Int = 0, arcBefore: Double = Double.MaxValue): Asteroid = {
    val a = asteroids.head

    if (count == 199) {
      a
    } else {
      if (a.arc == arcBefore) {
        traverse(asteroids.tail :+ asteroids.head, count, arcBefore)
      } else {
        traverse(asteroids.tail, count + 1, a.arc)
      }
    }
  }

//  (result._1 ::: result._2).zipWithIndex.foreach(a => print(s"${a._2+1} => ${a._1.position.swap} - ${a._1.arc} - ${a._1.distance} \n"))

  print(traverse(result._1 ::: result._2))
}
