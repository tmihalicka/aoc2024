package com.mihalicka.aoc2024

import scala.io.Source

object ResourceLoader:
  def loadResource(day: Int): List[String] =
    val resource = getClass.getResourceAsStream(s"/day_$day.txt")
    require(resource != null, s"Resource file for day: '$day' not found.")

    Source.fromInputStream(resource).getLines().toList






