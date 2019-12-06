var count = 0

for (i <- 171309 to 643603) {
  if (i.toString.groupBy(c => c).map(_._2.length).toSet.contains(2)
    && i.toString.sorted.toInt == i)
    count = count + 1
}

count
