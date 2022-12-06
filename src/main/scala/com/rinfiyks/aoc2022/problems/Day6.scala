package com.rinfiyks.aoc2022.problems

object Day6 extends Problem {

  override val day: Int = 6

  private def firstMarker(line: String, size: Int): Int = {
    line.sliding(size).zipWithIndex.find {
      case (l, _) =>
        l.toSet.size == size
    }.map(_._2).getOrElse(0) + size
  }
  def part1 = firstMarker(input.head, 4)
  def part2 = firstMarker(input.head, 14)

  println(part1)
  println(part2)

}
