package com.rinfiyks.aoc2022.problems

object Day8 extends Problem {

  override val day: Int = 8

  def part1(input: List[String]) = {
    val grid = input.map(_.toCharArray.toList.map(_.toInt))
    val rotated = input.transpose.map(_.map(_.toInt))

    var c = 0
    grid.indices.foreach { i =>
      rotated.indices.foreach { j =>

        val visibleL = grid(i).take(j).forall(_ < grid(i)(j))
        val visibleR = grid(i).drop(j + 1).forall(_ < grid(i)(j))
        val visibleU = rotated(j).take(i).forall(_ < rotated(j)(i))
        val visibleD = rotated(j).drop(i + 1).forall(_ < rotated(j)(i))
        if (visibleL || visibleR || visibleU || visibleD) {
          c += 1
        }
      }
    }
    c
  }

  def part2(input: List[String]) = {
    val grid = input.map(_.toCharArray.toList.map(_.toInt))
    val rotated = input.transpose.map(_.map(_.toInt))

    var max = 0
    grid.indices.foreach { i =>
      rotated.indices.foreach { j =>

        val treesL = grid(i).take(j).reverse
        val countL = math.min(treesL.length, treesL.takeWhile(_ < grid(i)(j)).length + 1)

        val treesR = grid(i).drop(j + 1)
        val countR = math.min(treesR.length, treesR.takeWhile(_ < grid(i)(j)).length + 1)

        val treesU = rotated(j).take(i).reverse
        val countU = math.min(treesU.length, treesU.takeWhile(_ < rotated(j)(i)).length + 1)

        val treesD = rotated(j).drop(i + 1)
        val countD = math.min(treesD.length, treesD.takeWhile(_ < rotated(j)(i)).length + 1)

        val score = countL * countR * countU * countD
        max = math.max(max, score)
      }
    }
    max

  }

  println(part1(input))
  println(part2(input))

}
