package com.rinfiyks.aoc2022.problems

object Day1 extends Problem {

  override val day: Int = 1

  private val parsed = chunkList(input, _.isEmpty)

  def part1 = parsed.map(_.map(_.toInt))
    .maxBy(_.sum).sum

  def part2 = parsed.map(_.map(_.toInt))
    .sortBy(_.sum).reverse.take(3).map(_.sum).sum

  println(part1)
  println(part2)

}
