package com.mihalicka.aoc2024

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * Day 3: Mull It Over
 * @see https://adventofcode.com/2024/day/3
 */
object Day3 extends AoC2024(3):

  private def puzzleData: String =
    puzzleInput
      .mkString("\n")
      .trim

  private def part1(): Unit =
    val pattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
    val matches = pattern.findAllMatchIn(puzzleData)

    val result = matches.map { m =>
      val (a, b) = (m.group(1).toInt, m.group(2).toInt)
      a * b
    }.sum

    println(s"Sum of all instruction: $result")


  private def part2(): Unit =
    val pattern = """(mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\))""".r

    val instructions = pattern.findAllMatchIn(puzzleData).toList

    @tailrec
    def process(instructions: List[Regex.Match], isEnabled: Boolean, sum: Int): Int =
      instructions match
        case Nil => sum // No more instructions to process
        case head :: tail =>
          head match
            case pattern(mul, x, y) if mul.startsWith("mul") =>
              val newSum = if (isEnabled) sum + x.toInt * y.toInt else sum
              process(tail, isEnabled, newSum)
            case pattern(doInstruction, _, _) if doInstruction == "do()" =>
              process(tail, isEnabled = true, sum)
            case pattern(dontInstruction, _, _) if dontInstruction == "don't()" =>
              process(tail, isEnabled = false, sum)
            case _ =>
              process(tail, isEnabled, sum) // Skip other instructions

    val resultSum = process(instructions, isEnabled = true, sum = 0)

    println(s"Sum of valid enabled mul instructions: $resultSum")

  @main def runDay3(): Unit =
    part1()
    part2()
