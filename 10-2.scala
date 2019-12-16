import com.sun.tools.classfile.TypeAnnotation.Position

object Main extends App {
  val input: Seq[List[Char]] = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##".split("\n").toList.map(_.toList)
  case class Asteroid(position: (Int, Int), distance: Double, arc: Double, group: Int = -1)

  def analyze(position: (Int, Int)) = {
    input.zipWithIndex.foldLeft(List.empty[Asteroid]) {
      case (asteroids, (row, y)) =>
        row.zipWithIndex.filter(cx => cx._1 == '#').map(_._2).foldLeft(asteroids) {
          case (asteroids, x) if position == (y, x) => asteroids
          case (asteroids, x) =>
            val arc = Math.atan2(position._2-y, position._1-x)
            val distance = Math.sqrt((position._1-x)*(position._1-x) + (position._2-y)*(position._2-y))

            asteroids :+ Asteroid((y, x), distance, arc)
        }
    }
  }

  //  val result = analyze((25,31)).sortBy(_.distance).groupBy(_.arc).toSeq.sortBy(_._1).map(x => x._1 -> x._2.sortBy(_.distance)).partition(_._1 >= 1.5707963267948966)

  val result = analyze(8, 3).sortBy(a => (a.arc, a.distance)).partition(_.arc >= Math.PI/2)

  @scala.annotation.tailrec
  def traverse(asteroids: List[Asteroid], count: Int = 0, arcBefore: Double = Double.MaxValue): Asteroid = {
    val a = asteroids.head

    if (count == 200) {
      a
    } else {
      if (a.arc == arcBefore) {
        traverse(asteroids.tail :+ asteroids.head, count, arcBefore)
      } else {
        print(a.position, "\n")
        traverse(asteroids.tail, count + 1, a.arc)
      }
    }
  }
//
//  print(traverse((result._2 ::: result._1).reverse, 1).position) // TODO check with reverse

  (result._2 ::: result._1).foreach(a => print(a, "\n"))
}
