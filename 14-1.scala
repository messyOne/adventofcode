import Main.reactions

object Main extends App {
  val input = "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF"
  val reactions = input.split("\n").map(_.trim).foldLeft(List.empty[Reaction]) {
    case (reactions, data) =>
      val dataSplit = data.split("=>").map(_.trim)
      val input: List[Chemical] = parseChemicals(dataSplit.head)
      val output: Chemical = parseChemicals(dataSplit.last).head

      reactions :+ Reaction(input, output)
  }

  val fuelReaction = reactions.find(_.output.isFuel).getOrElse(throw new RuntimeException("No reaction found"))
  val oreReactions = reactions.filter(_.input.exists(_.isOre))
  val oreChemicals = oreReactions.map(_.output.name)
  val reducedReactions = reduce(fuelReaction.input)
    .groupBy(_.name).map(kvp => Chemical(kvp._1, kvp._2.map(_.amount).sum))

  val total = reducedReactions.foldLeft(0) {
    case (sum, chemical) =>
//      val reaction = oreReactions.find(_.output.name == chemical.name).getOrElse(throw new RuntimeException("No reaction found"))
      val reaction = find(chemical.name)
//      Chemical(reaction.input.head.name,reaction.input.head.amount * (head.amount / reaction.output.amount).ceil)

      (chemical.amount / reaction.output.amount).ceil.toInt * reaction.input.head.amount.toInt + sum
  }

  print(s"Total Ore: ${total}")

  def parseChemicals(dataSplit: String): List[Chemical] = {
    dataSplit.split(',').foldLeft(List.empty[Chemical]) {
      case (list, data) => val dataSplit = data.trim.split(" ").map(_.trim)
        list :+ Chemical(dataSplit.last, dataSplit.head.toInt)
    }
  }

  // TODO replace each reaction with it's inputs until only ores are left. than just calculate
  // TODO make reactions and oreReactions class member
  def reduce(chemicals: List[Chemical], leftovers: List[Chemical] = Nil): List[Chemical] = {
    //    val chemicals = c.groupBy(_.name).map(kvp => Chemical(kvp._1, kvp._2.map(_.amount).sum)).toList

    chemicals match {
      case ::(head, tail) if oreChemicals.contains(head.name) =>
        reduce(tail, leftovers) :+ head
      //        val reaction = find(reactions, head.name)
      //
      //        reduce(reactions, oreReactions, tail) :+ Chemical(reaction.input.head.name,reaction.input.head.amount * (head.amount / reaction.output.amount).ceil)
      case ::(head, tail) =>
        val replaceReaction = find(head.name)
        val amount = head.amount - leftovers.find(_.name == head.name).map(_.amount).getOrElse(0f)
        val neededAmount = (amount / replaceReaction.output.amount).ceil
        val leftoverAmount = replaceReaction.output.amount % amount
        val replaceChemicals = replaceReaction.input.map(c => c.copy(amount = c.amount * neededAmount))

        val leftover = leftovers.find(_.name == head.name) match {
          case Some(chemical) => chemical.copy(amount = chemical.amount + leftoverAmount)
          case None => Chemical(head.name, leftoverAmount)
        }

        // KHKGT
        // c.amount => 3, head.amount => 5, replaceReaction.output.amount = 8

        reduce(tail ::: replaceChemicals, leftovers.filter(_.name != head.name) :+ leftover)
      case Nil => chemicals
    }
  }

  def find(name: String): Reaction = {
    reactions.find(_.output.name == name).getOrElse(throw new RuntimeException(s"Reaction ${name} does not exist"))
  }
}

case class Reaction(input: List[Chemical], output: Chemical)

case class Chemical(name: String, amount: Float) {
  def isFuel: Boolean = name == "FUEL"

  def isOre: Boolean = name == "ORE"

  def inc(by: Int): Chemical = copy(amount = amount + by)
}
