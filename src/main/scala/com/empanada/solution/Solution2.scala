package com.empanada.solution

import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}

object Solution2 extends IOApp {
  sealed trait Scorable { def score: Int = scoringSystem(this) }
  sealed trait Shape extends Scorable {
    def vs(hand: Shape): Outcome = outcomes(this, hand)
  }
  sealed trait Outcome extends Scorable
  final case class Match(they: Shape, me: Shape) extends Scorable
  object Shape {
    final case object Rock extends Shape
    final case object Paper extends Shape
    final case object Scissors extends Shape

    def fromEncrypted(encrypted: String): Shape = encrypted match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }
    def against(theirShape: Shape, expectedResult: String): Shape =
      expectedResult match {
        case "X" => lossesAgainst(theirShape)
        case "Y" => theirShape // Draw
        case "Z" => winsAgainst(theirShape)
      }

    private val lossesAgainst: Map[Shape, Shape] = Map(
      Rock -> Scissors,
      Paper -> Rock,
      Scissors -> Paper
    )
    private val winsAgainst: Map[Shape, Shape] = Map(
      Rock -> Paper,
      Paper -> Scissors,
      Scissors -> Rock
    )
  }
  object Outcome {
    final case object Victory extends Outcome
    final case object Defeat extends Outcome
    final case object Draw extends Outcome
  }
  private object Match {
    def fromEncrypted(encrypted: String): Match = {
      val msg = encrypted.split(" ")
      require(msg.size == 2)
      val (they, expectedResult) = (msg.head, msg.last)
      val theirShape = Shape.fromEncrypted(they)
      val myShape = Shape.against(theirShape, expectedResult)
      Match(theirShape, myShape)
    }
  }
  import Shape._
  import Outcome._
  private val scoringSystem: Map[Scorable, Int] = Map(
    Rock -> 1,
    Paper -> 2,
    Scissors -> 3,
    Victory -> 6,
    Draw -> 3,
    Defeat -> 0
  )

  private val scoreMatch: Match => Int = m => m.me.vs(m.they).score + m.me.score

  private val outcomes: Map[(Shape, Shape), Outcome] = Map(
    (Rock, Rock) -> Draw,
    (Rock, Paper) -> Defeat,
    (Rock, Scissors) -> Victory,
    (Paper, Paper) -> Draw,
    (Paper, Scissors) -> Defeat,
    (Paper, Rock) -> Victory,
    (Scissors, Scissors) -> Draw,
    (Scissors, Rock) -> Defeat,
    (Scissors, Paper) -> Victory
  )

  private def readInput(pathStr: String): fs2.Stream[IO, String] = {
    val path = Path.apply(pathStr)
    Files
      .apply[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
  }

  def process(input: fs2.Stream[IO, String]): IO[Int] =
    input
      .filter(_.nonEmpty)
      .map(Match.fromEncrypted)
      .map(scoreMatch)
      .fold(0)(_ + _)
      .compile
      .onlyOrError

  def run(filepath: String): IO[Int] = {
    val input = readInput(filepath)
    process(input)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-2.txt"
    run(filePath)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
