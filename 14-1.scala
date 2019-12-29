import Main.reactions

object Main extends App {
  val input = "5 LKQCJ, 1 GDSDP, 2 HPXCL => 9 LVRSZ\n5 HPXCL, 5 PVJGF => 3 KZRTJ\n7 LVRSZ, 2 GFSZ => 5 FRWGJ\n9 ZPTXL, 5 HGXJH, 9 LQMT => 7 LVCXN\n2 LQMT, 2 PVJGF, 10 CKRVN => 9 VWJS\n2 VRMXL, 12 NBRCS, 2 WSXN => 7 GDSDP\n1 CKRP => 8 TBHVH\n1 SVMNB, 2 KZRTJ => 8 WKGQS\n6 LKQCJ, 8 HPXCL, 7 MPZH => 1 BQPG\n1 RCWL => 7 MPZH\n4 FGCMS, 2 LQMT, 1 LKQCJ => 1 KTBRM\n1 ZTCSK, 6 CXQB, 2 ZBZRT => 3 PVJGF\n7 DBNLM => 9 ZBZRT\n5 BGNQ, 2 WBPD, 5 KTBRM => 9 GFSZ\n6 XQBHG, 1 GPWVC => 8 CKFTS\n1 XWLQM, 29 XQBHG, 7 KPNWG => 5 BXVL\n6 TBHVH, 1 KTBRM => 7 HJGR\n1 LQMT, 14 KPNWG => 7 GPWVC\n18 LVCXN, 8 XVLT, 4 KPNWG, 13 LKQCJ, 12 MFJFW, 5 GZNJZ, 1 FLFT, 7 WBPD => 8 KZGD\n1 TBHVH => 1 VWKJ\n118 ORE => 2 CKRP\n2 LTCQX => 3 XQBHG\n1 GPWVC => 4 SMFQ\n6 CKRP => 4 RCWL\n39 LHZMD, 15 CKFTS, 26 HVBW, 57 KTBRM, 13 DFCM, 30 KZGD, 35 FPNB, 1 LKQCJ, 45 HJGR, 22 RCZS, 34 VWKJ => 1 FUEL\n1 BQPG, 2 BGNQ, 12 WBPD => 8 LTCQX\n2 WSXN => 2 HPXCL\n3 GRFPX => 5 XVLT\n1 LVRSZ => 3 SVMNB\n6 HLMT => 9 ZPTXL\n20 GFSZ => 5 GZNJZ\n1 RCWL => 9 KPNWG\n24 BGNQ, 31 KTBRM => 8 FLFT\n14 VSVG => 9 DBNLM\n191 ORE => 8 CXQB\n115 ORE => 2 SWVLZ\n17 KZRTJ, 13 KPNWG => 7 CKRVN\n9 BQPG => 4 XWLQM\n4 SMFQ, 2 GRFPX => 1 MFJFW\n6 CXQB, 4 CKRP, 2 BXVL, 5 GZNJZ, 3 VWJS, 1 FLFT, 4 KPNWG => 7 DFCM\n1 TBHVH => 6 BGNQ\n3 LQMT => 7 HLMT\n11 GDSDP => 4 WBPD\n2 KPNWG, 5 VWJS, 33 NBRCS => 7 NVDW\n5 GDSDP => 6 FGCMS\n1 GPWVC, 7 BGNQ, 1 FRWGJ => 8 GRFPX\n23 KTBRM, 11 VRMXL, 6 GPWVC => 5 SRJHK\n2 XQBHG, 1 GZNJZ => 3 HVBW\n1 ZTCSK => 4 WSXN\n1 XVLT, 5 HLMT, 1 ZPTXL, 2 HVBW, 7 NVDW, 1 WKGQS, 1 LTCQX, 5 MPZH => 3 FPNB\n16 SRJHK => 6 DWBW\n1 SVMNB, 1 VRMXL => 3 HGXJH\n133 ORE => 6 VSVG\n3 NBRCS, 1 FGCMS => 4 LQMT\n1 CKRP => 4 ZTCSK\n5 CKRVN, 1 FLFT => 1 RCZS\n4 ZTCSK, 15 RCWL => 9 LKQCJ\n1 SWVLZ => 8 NBRCS\n5 CKRP, 14 CXQB => 5 VRMXL\n1 SMFQ, 1 DWBW => 2 LHZMD"
  val reactions = input.split("\n").map(_.trim).foldLeft(List.empty[Reaction]) {
    case (reactions, data) =>
      val dataSplit = data.split("=>").map(_.trim)
      val input: List[Chemical] = parseChemicals(dataSplit.head)
      val output: Chemical = parseChemicals(dataSplit.last).head

      reactions :+ Reaction(input, output)
  }

  val fuelReaction = reactions.find(_.output.isFuel).getOrElse(throw new RuntimeException("No reaction found"))
  val total = reduce(fuelReaction.input)

  print(s"Total Ore: ${total}, (${total < 369690 && total != 365775})")

  def parseChemicals(dataSplit: String): List[Chemical] = {
    dataSplit.split(',').foldLeft(List.empty[Chemical]) {
      case (list, data) => val dataSplit = data.trim.split(" ").map(_.trim)
        list :+ Chemical(dataSplit.last, dataSplit.head.toInt)
    }
  }

  def reduce(c: List[Chemical], leftovers: List[Chemical] = Nil): Double = {
    val chemicals = c.groupBy(_.name).map(kvp => Chemical(kvp._1, kvp._2.map(_.amount).sum)).toList

    chemicals match {
      case ::(head, tail) if head.isOre => head.amount + reduce(tail, leftovers)
      case ::(head, tail) =>
        val replaceReaction = find(head)
        val beforeLeftover = leftovers.find(_.name == head.name).getOrElse(Chemical(head.name, 0))
        val newLeftover = beforeLeftover.copy(amount = Math.max(beforeLeftover.amount - head.amount, 0))
        val amount = Math.max(head.amount - beforeLeftover.amount, 0)
        val neededAmount = if (replaceReaction.output.amount == 0) 1 else (amount / replaceReaction.output.amount).ceil
        val leftoverAmount = if (amount != 0) (replaceReaction.output.amount * neededAmount) % amount else 0
        val replaceChemicals = replaceReaction.input.map(c => c.copy(amount = c.amount * neededAmount))

//        val leftover = leftovers.find(_.name == head.name) match {
//          case Some(chemical) => chemical.copy(amount = chemical.amount + leftoverAmount - Math.max(beforeLeftover - head.amount, 0))
//          case None => Chemical(head.name, leftoverAmount)
//        }

        reduce(tail ::: replaceChemicals, leftovers.filter(_.name != head.name) :+ newLeftover.copy(amount = newLeftover.amount + leftoverAmount))
      case Nil => 0
    }
  }

  def find(chemical: Chemical): Reaction = {
    reactions.find(_.output.name == chemical.name).getOrElse(Reaction(List(chemical), chemical))
  }
}

case class Reaction(input: List[Chemical], output: Chemical)

case class Chemical(name: String, amount: Double) {
  def isFuel: Boolean = name == "FUEL"

  def isOre: Boolean = name == "ORE"
}
