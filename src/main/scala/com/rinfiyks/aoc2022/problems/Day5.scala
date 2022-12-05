package com.rinfiyks.aoc2022.problems

import scala.annotation.tailrec

object Day5 extends Problem {

  override val day: Int = 5

  private def parse(drawing: List[String]) = {
    val numStacks = drawing.last.split(" ").last.toInt
    val stacksDrawing = drawing.dropRight(1)
    stacksDrawing.foldLeft(List.fill(numStacks)("")) {
      case (stacks, row) =>
        stacks.zipWithIndex.map {
          case (s, i) =>
            s + row.lift(i * 4 + 1).map(_.toString).getOrElse("").trim
        }
    }
  }

  @tailrec
  private def doMovements(stacks: List[String], commands: List[String], rev: Boolean): List[String] = {
    if (commands.isEmpty) stacks
    else {
      val command = commands.head.split(" ")
      val numToMove = command(1).toInt
      val from = command(3).toInt - 1
      val to = command(5).toInt - 1
      val letters =
        if (rev) stacks(from).take(numToMove).reverse
        else stacks(from).take(numToMove)

      val newStacks = stacks.zipWithIndex.map {
        case (s, i) =>
          if (i == from) s.drop(numToMove)
          else if (i == to) letters + s
          else s
      }

      doMovements(newStacks, commands.tail, rev)
    }
  }


  def part1 = {
    val drawing = input.takeWhile(_.nonEmpty)
    val commands = input.dropWhile(_.nonEmpty).tail
    val stacks = parse(drawing)
    val output = doMovements(stacks, commands, true)
    output.map(_.head).mkString
  }

  def part2 = {
    val drawing = input.takeWhile(_.nonEmpty)
    val commands = input.dropWhile(_.nonEmpty).tail
    val stacks = parse(drawing)
    val output = doMovements(stacks, commands, false)
    output.map(_.head).mkString
  }

  println(part1)
  println(part2)

}
