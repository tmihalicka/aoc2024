package com.mihalicka.aoc2024

/**
 * Day 2: Red-Nosed Reports
 * @see https://adventofcode.com/2024/day/2
 */
object Day2 extends AoC2024(2):

  private def puzzleData: List[List[Int]] =
    puzzleInput
      .map(_.split(" ").map(_.toInt).toList)

  private def isSafe(input: List[Int]): Boolean =
    val diff = input.sliding(2).map { case Seq(a, b) => a - b }.toList
    diff.forall(d => d >= 1 && d <= 3) || diff.forall(d => d <= -1 && d >= -3)

  private def part1(): Unit =
    val safeReports = puzzleData.count(isSafe)
    println("Safe reports: " + safeReports)

  private def part2(): Unit =
    def isReportSafeWithDampener(report: List[Int]): Boolean =
      report.indices.exists { i =>
        val modified = report.take(i) ++ report.drop(i + 1)
        isSafe(modified)
      }

    val safeDempeneredReports = puzzleData.count { report =>
      isSafe(report) || isReportSafeWithDampener(report)
    }

    println("Dampenered safe reports: " + safeDempeneredReports)


  @main def runDay2(): Unit =
    part1()
    part2()
