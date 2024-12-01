package com.mihalicka.aoc2024

/**
 * Day 1: Historian Hysteria
 * @see https://adventofcode.com/2024/day/1
 */
object Day1:

  private def inputLines: List[(Int, Int)] =
    ResourceLoader
      .loadResource(1)
      .map(_.trim.split("\\s+").map(_.toInt))
      .map { case Array(left, right) => (left, right) }

  private def solutionPart1: Int =
    val (left, right) = inputLines.unzip

    left
      .sorted
      .zip(right.sorted)
      .map((left, right) => math.abs(left - right))
      .sum

  private def solutionPar2: Int =
    val (left, right) = inputLines.unzip

    val rightFrequency = right.groupBy(identity).view.mapValues(_.size).toMap

    left
      .map(num => num * rightFrequency.getOrElse(num, 0)).sum


  @main def run() =
    println(s"Total distance: $solutionPart1")
    println(s"Similarity Score: $solutionPar2")






