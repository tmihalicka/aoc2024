package com.mihalicka.aoc2024

import scala.annotation.tailrec

object Day4 extends AoC2024(4):

  private def puzzleData: List[String] =
    puzzleInput
      .map(_.trim)

  private def part1(): Unit =
    val grid = puzzleData
    val rows = grid.length
    val cols = grid.head.length

    val target = "XMAS"
    val directions =
      List(
        (0, 1), // Right
        (0, -1), // Left
        (1, 0), // Down
        (-1, 0), // Up
        (1, 1), // Down-Right
        (1, -1), // Down-Left
        (-1, 1), // Up-Right
        (-1, -1), // Up-Left
      )

    def isValid(x: Int, y: Int): Boolean =
      x >= 0 && x < cols && y >= 0 && y < cols

    @tailrec
    def search(x: Int, y: Int, wordIndex: Int, dx: Int, dy: Int): Boolean =
      if wordIndex == target.length then true
      else if !isValid(x, y) then false
      else if grid(y)(x) != target(wordIndex) then false
      else search(x + dx, y + dy, wordIndex + 1, dx, dy)

    def countFromCell(x: Int, y: Int): Int =
      directions.map { case (dx, dy) =>
        if search(x, y, 0, dx, dy) then 1 else 0
      }.sum

    val result = (for
      x <- 0 until cols
      y <- 0 until rows
    yield countFromCell(x, y)).sum

    println(s"Total occurrences of $target: $result")

  private def part2(): Unit = ???


  @main def runDay4(): Unit =
    part1()
//    part2()


