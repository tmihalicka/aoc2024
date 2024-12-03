package com.mihalicka.aoc2024

/**
 * Day 1: Historian Hysteria
 * @see https://adventofcode.com/2024/day/1
 */
object Day1 extends AoC2024(1):

  private def puzzleData: List[(Int, Int)] =
    puzzleInput
      .map(_.trim.split("\\s+").map(_.toInt))
      .map { case Array(left, right) => (left, right) }

  private def part1(): Unit =
    val (left, right) = puzzleData.unzip

    val result = left
      .sorted
      .zip(right.sorted)
      .map((left, right) => math.abs(left - right))
      .sum

    println(s"Total distance: $result")

  private def part2(): Unit =
    val (left, right) = puzzleData.unzip

    val rightFrequency = right.groupBy(identity).view.mapValues(_.size).toMap

    val result = left
      .map(num => num * rightFrequency.getOrElse(num, 0)).sum

    println(s"Similarity Score: $result")

  @main def runDay1(): Unit =
    part1()
    part2()