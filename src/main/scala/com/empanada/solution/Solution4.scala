package com.empanada.solution

import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}

object Solution4 extends IOApp {
  object Sections {
    final case class Sections(value: List[Int]) {
      def fullyContains(setB: Sections): Boolean =
        isContainedIn(setB, this)

      def overlaps(setB: Sections): Boolean = isOverlapping(this, setB)
    }

    def containsOneOrAnother(one: Sections, other: Sections): Boolean =
      isContainedIn(one, other) || isContainedIn(other, one)

    def fromTwoElves(input: String): (Sections, Sections) = {
      val elvesSections = input.split(",")
      val (sections1, sections2) = (elvesSections.head, elvesSections.last)
      buildSections(sections1) -> buildSections(sections2)
    }

    def buildSections(input: String): Sections = {
      val sections = input.split("-").map(_.toInt)
      val (from, to) = (sections.head, sections.last)
      range(from, to)
    }

    private def range(from: Int, to: Int): Sections =
      Sections(List.range(from, to + 1))

    private def isContainedIn(s1: Sections, s2: Sections): Boolean =
      s1.value.forall(s => s2.value.contains(s))

    private def isOverlapping(s1: Sections, s2: Sections): Boolean =
      s1.value.exists(s => s2.value.contains(s))

  }

  def readInput(pathStr: String): fs2.Stream[IO, String] = {
    val path = Path.apply(pathStr)
    Files
      .apply[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .filter(_.nonEmpty)
  }

  private def countFullyContained(input: fs2.Stream[IO, String]): IO[Int] = {
    input
      .map(Sections.fromTwoElves)
      .foldMap(sections => countContained(sections._1, sections._2))
      .compile
      .onlyOrError
  }

  private def countOverlappingSections(
      input: fs2.Stream[IO, String]
  ): IO[Int] = {
    input
      .map(Sections.fromTwoElves)
      .foldMap(sections => countOverlap(sections._1, sections._2))
      .compile
      .onlyOrError
  }

  private def countOverlap(s1: Sections.Sections, s2: Sections.Sections): Int =
    if (s1.overlaps(s2)) 1 else 0

  private def countContained(
      s1: Sections.Sections,
      s2: Sections.Sections
  ): Int = {
    if (Sections.containsOneOrAnother(s1, s2)) 1 else 0
  }

  def countFullyContainedSections(filepath: String): IO[Int] = {
    val input = readInput(filepath)
    countFullyContained(input)
      .flatTap(IO.println)
  }

  def countOverlappedSections(filepath: String): IO[Int] = {
    val input = readInput(filepath)
    countOverlappingSections(input)
      .flatTap(IO.println)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-4.txt"
    countOverlappedSections(filePath).as(ExitCode.Success)
  }

}
