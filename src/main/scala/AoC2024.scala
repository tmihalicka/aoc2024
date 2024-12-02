package com.mihalicka.aoc2024

import scala.io.Source

trait AoC2024(day: Int):
  def puzzleInput: List[String] =
    val resource = getClass.getResourceAsStream(s"/day_$day.txt")
    require(resource != null, s"Resource file for day: '$day' not found.")

    Source.fromInputStream(resource).getLines().toList
