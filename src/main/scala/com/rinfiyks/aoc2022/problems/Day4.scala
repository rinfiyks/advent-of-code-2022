package com.rinfiyks.aoc2022.problems

object Day4 extends Problem {

  override val day: Int = 4

  private def containsOther(s: String): Boolean = {
    val e1 = s.split(",")(0)
    val e2 = s.split(",")(1)

    val e1start = e1.split("-")(0).toInt
    val e1end = e1.split("-")(1).toInt

    val e2start = e2.split("-")(0).toInt
    val e2end = e2.split("-")(1).toInt
    (e1start <= e2start && e1end >= e2end) || (e2start <= e1start && e2end >= e1end)
  }

  private def overlapsOther(s: String): Boolean = {
    val e1 = s.split(",")(0)
    val e2 = s.split(",")(1)

    val e1start = e1.split("-")(0).toInt
    val e1end = e1.split("-")(1).toInt

    val e2start = e2.split("-")(0).toInt
    val e2end = e2.split("-")(1).toInt
    (e1start >= e2start && e1start <= e2end) || (e1end >= e2start && e1end <= e2end)
      || (e2start >= e1start && e2start <= e1end) || (e2end >= e1start && e2end <= e1end)
  }

  def part1 = input.map(containsOther).count(_ == true)

  def part2 = input.map(overlapsOther).count(_ == true)

  println(part1)
  println(part2)

}
