package com.rinfiyks.aoc2022.problems

object Day3 extends Problem {

  override val day: Int = 3

  private def priorityOfDuplicate(rucksack: String): Int = {
    val (r1, r2) = rucksack.splitAt(rucksack.length / 2)
    val duplicate = (r1 intersect r2).head
    if (duplicate.isUpper) duplicate - 'A' + 27
    else duplicate - 'a' + 1
  }

  private def priorityOfGroup(rucksacks: List[String]): Int = {
    val duplicate = (rucksacks(0) intersect rucksacks(1) intersect rucksacks(2)).head
    if (duplicate.isUpper) duplicate - 'A' + 27
    else duplicate - 'a' + 1
  }

  def part1 = input.map(priorityOfDuplicate).sum

  def part2 = input.grouped(3).toList.map(priorityOfGroup).sum

  println(part1)
  println(part2)

}
