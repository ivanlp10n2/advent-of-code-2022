package com.empanada.solution

import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}

object Solution4 extends IOApp {
  object Sections {
    final case class Sections(value: List[Int]){
      def contains(setB: Sections): Boolean =
        isContainedIn(setB, this)
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

  /** Is a section contained in the other or */
  private def countOverlaps(input: fs2.Stream[IO, String]): IO[Int] = {
    input
      .map(Sections.fromTwoElves)
      .foldMap(sections => countOverlap(sections._1, sections._2))
      .compile
      .onlyOrError
  }

  private def countOverlap(
      s1: Sections.Sections,
      s2: Sections.Sections
  ): Int = {
    if (Sections.containsOneOrAnother(s1, s2)) 1 else 0
  }

  def run(filepath: String): IO[Int] = {
    val input = readInput(filepath)
    countOverlaps(input)
      .flatTap(IO.println)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-4.txt"
    run(filePath).as(ExitCode.Success)
  }

}
