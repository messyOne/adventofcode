var count = 0

for (i <- 171309 to 643603) {
  if ("(.)\\1+".r.findFirstIn(i.toString).isDefined && i.toString.sorted.toInt == i)
    count = count + 1
}

count
