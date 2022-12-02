package com.rinfiyks.aoc2022.problems

object Day2 extends Problem {

  override val day: Int = 2

  private def score1(round: String): Int = {
    val p1 = round(0) - 'A'
    val p2 = round(2) - 'X'
    // 0 = rock, 1 = scissors, 2 = paper

    val result = (3 + p1 - p2) % 3 // 0=tie, 1=loss, 2=win

    if (result == 0) {
      p2 + 1 + 3
    } else if (result == 1) {
      p2 + 1 + 0
    } else {
      p2 + 1 + 6
    }
  }

  private def score2(round: String): Int = {
    val p1 = round(0) - 'A'
    val result = round(2) - 'X'
    // 0 = lose, 1 = draw, 2 = win

    val p2 = if (result == 0) {
      (p1 + 2) % 3
    } else if (result == 1) {
      p1
    } else {
      (p1 + 1) % 3
    }

    if (result == 0) {
      p2 + 1 + 0
    } else if (result == 1) {
      p2 + 1 + 3
    } else {
      p2 + 1 + 6
    }
  }

  def part1 = input.map(score1).sum
  def part2 = input.map(score2).sum

  println(part1)
  println(part2)

}
