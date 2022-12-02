package com.rinfiyks.aoc2022.problems

trait Problem extends App {

  val day: Int

  lazy val input: List[String] = io.Source.fromResource(s"puzzles/day$day.txt").getLines.toList

  def time[A](f: => A): A = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    println(s"${end - start} ms")
    result
  }

  def chunkList[A](s: List[A], p: A => Boolean): List[List[A]] = {
    def go(s: List[A], res: List[List[A]]): List[List[A]] = {
      val (h, t) = s.span(!p(_))
      if (t.isEmpty) h :: res
      else go(t.dropWhile(p), h :: res)
    }

    go(s, List.empty[List[A]])
  }

}
