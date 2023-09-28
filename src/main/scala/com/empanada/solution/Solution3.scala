package com.empanada.solution

import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}

object Solution3 extends IOApp {
  final case class Rucksack private (items: List[Char]) {
    def sumDuplicatedTypePriorities: Int = {
      val duplicatedTypes = duplicatedItems.toSet
      val sumPriorities = duplicatedTypes
        .foldLeft(0)((acc, char) => acc + Item.toPriority(char))
      sumPriorities
    }

    private def compartments: (List[Char], List[Char]) = {
      val halfSize = items.size / 2
      items.splitAt(halfSize)
    }

    def duplicatedItems: List[Char] = {
      val (first, second) = compartments
      first.intersect(second)
    }
  }
  object Rucksack {
    def of(items: String): Rucksack = new Rucksack(items.toList)

  }
  object Item {
    private val lowerAlphabet: Array[Char] =
      "abcdefghijklmnopqrstuvwxyz".toCharArray
    private val upperAlphabet: Array[Char] = lowerAlphabet.map(_.toUpper)

    private def applyPriorities(alphabet: Iterable[Char]): Map[Char, Int] =
      alphabet.zipWithIndex.map { case (letter, index) =>
        val priority = index + 1
        letter -> priority
      }.toMap

    private val priorities: Map[Char, Int] = applyPriorities(
      lowerAlphabet ++ upperAlphabet
    )
    def toPriority(char: Char): Int = priorities(char)
    def sumPriorities(items: List[Char]): Int =
      items.foldLeft(0)((acc, char) => acc + priorities(char))
  }

  private def readInput(pathStr: String): fs2.Stream[IO, String] = {
    val path = Path.apply(pathStr)
    Files
      .apply[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
  }

  private def process(input: fs2.Stream[IO, String]): IO[Int] = {
    input
      .map(Rucksack.of)
      .foldMap(_.sumDuplicatedTypePriorities)
      .compile
      .onlyOrError
  }

  def run(filepath: String): IO[Int] = {
    val input = readInput(filepath)
    process(input)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-3.txt"
    run(filePath).as(ExitCode.Success)
  }

}
