package com.empanada.solution

import cats.effect
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Chunk
import fs2.io.file.{Files, Path}

object Solution3FP extends IOApp {
  final case class Rucksack private (items: List[Char]) {
    lazy val compartments: (List[Char], List[Char]) =
      items.splitAt(items.size / 2)

    lazy val duplicates: List[Char] =
      Rucksack.findDuplicates(compartments._1, compartments._2)
  }
  object Rucksack {
    def of(items: String): Rucksack = new Rucksack(items.toList)
    private def findDuplicates(
        oneCompartment: List[Char],
        anotherCompartment: List[Char]
    ): List[Char] =
      oneCompartment.intersect(anotherCompartment)

    def findDuplicates(elves: List[Rucksack]): List[Char] = {
      require(elves.size == 3, elves.mkString("[", ",", "]"))
      elves match {
        case elf1 :: elf2 :: elf3 :: Nil => intersectElves(elf1, elf2, elf3)
      }
    }

    private def intersectElves(
        elf1: Rucksack,
        elf2: Rucksack,
        elf3: Rucksack
    ): List[Char] =
      elf1.items.intersect(elf2.items).intersect(elf3.items)
  }
  object Item {
    private val lowerAlphabet = 'a' to 'z'
    private val upperAlphabet = 'A' to 'Z'

    private def applyPriorities(alphabet: Iterable[Char]): Map[Char, Int] =
      alphabet.zipWithIndex.map { case (letter, index) =>
        val priority = index + 1
        letter -> priority
      }.toMap

    private val priorities: Map[Char, Int] = applyPriorities(
      lowerAlphabet ++ upperAlphabet
    )
    def toPriority(char: Char): Int = priorities(char)
    def sumPriorities(items: List[Char]): Int = items.map(priorities).sum
  }

  def sumDuplicatedTypePriorities(rugback: Rucksack): Int = {
    val duplicatedItems = rugback.duplicates
    val duplicatedTypes = duplicatedItems.toSet
    duplicatedTypes.map(Item.toPriority).sum
  }

  private def readInput(pathStr: String): fs2.Stream[IO, String] = {
    val path = Path.apply(pathStr)
    Files
      .apply[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .filter(_.nonEmpty)
  }

  private def processPriorityTypes(
      input: fs2.Stream[IO, String]
  ): fs2.Stream[IO, Int] =
    input
      .map(Rucksack.of)
      .foldMap(sumDuplicatedTypePriorities)

  private def groupRucksack(
      input: fs2.Stream[IO, String]
  ): fs2.Stream[IO, Chunk[Rucksack]] =
    input
      .map(Rucksack.of)
      .chunkN(3, false)

  def runPriorityTypesSum(filepath: String): IO[Int] = {
    readInput(filepath)
      .through(processPriorityTypes)
      .compile
      .onlyOrError
  }

  def runPriorityBadgeSum(filepath: String): IO[Int] = {
    val emptyCollection = IO.ref[List[List[Char]]](List.empty)

    def sumAllPriorities(ref: effect.Ref[IO, List[List[Char]]]): IO[Int] =
      ref.get.map {
        _.map(_.toSet).flatten
          .map(Item.toPriority)
          .sum
      }

    emptyCollection.flatMap(itemsCollection =>
      readInput(filepath)
        .through(groupRucksack)
        .through(findDuplicates)
        .evalTap(duplicates => itemsCollection.update(c => duplicates :: c))
        .compile
        .drain
        .flatMap(_ => sumAllPriorities(itemsCollection))
    )

  }

  private def findDuplicates
      : fs2.Stream[IO, Chunk[Rucksack]] => fs2.Stream[IO, List[Char]] =
    _.map(chunk => Rucksack.findDuplicates(chunk.toList))

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-3.txt"
    runPriorityBadgeSum(filePath)
      .flatMap(IO.println)
      .as(ExitCode.Success)
  }

}
