package com.rinfiyks.aoc2022.problems

import scala.collection.mutable.ListBuffer

object Day7 extends Problem {

  case class File(name: String, size: Int)

  case class Dir(name: String, parent: Option[Dir], subdirs: ListBuffer[Dir], files: ListBuffer[File]) {
    def size: Int = {
      subdirs.map(_.size).sum + files.map(_.size).sum
    }
  }

  override val day: Int = 7

  def parse(commands: List[String]): ListBuffer[Dir] = {
    val rootDir = Dir("/", None, ListBuffer.empty, ListBuffer.empty)
    val allDirs = ListBuffer(rootDir)
    var currentDir = rootDir
    commands.drop(1).foreach { line =>

      if (line.startsWith("dir")) {
        val dir = Dir(line.split(" ")(1), Some(currentDir), ListBuffer.empty, ListBuffer.empty)
        currentDir.subdirs.prepend(dir)
        allDirs.prepend(dir) // assuming we never run "$ dir" twice on the same directory

      } else if (line.matches("^[0-9].*")) {
        val file = File(line.split(" ")(1), line.split(" ")(0).toInt)
        currentDir.files.prepend(file)

      } else if (line == "$ cd ..") {
        currentDir = currentDir.parent.get
      }

      else if (line.startsWith("$ cd")) {
        val dirNameToCd = line.split(" ")(2)
        currentDir = currentDir.subdirs.find(_.name == dirNameToCd).get
      }
    }

    allDirs
  }

  def part1(input: List[String]) = {
    val allDirs = parse(input)

    allDirs.filter(_.size <= 100000).map(_.size).sum
  }

  def part2(input: List[String]) = {
    val allDirs = parse(input)
    val rootDir = allDirs.last

    val totalSpace = 70000000
    val requiredFreeSpace = 30000000
    val currentFreeSpace = totalSpace - rootDir.size
    val amountNeededToDelete = requiredFreeSpace - currentFreeSpace
    allDirs.sortBy(_.size).find(_.size >= amountNeededToDelete).get.size
  }

  println(part1(input))
  println(part2(input))

}
