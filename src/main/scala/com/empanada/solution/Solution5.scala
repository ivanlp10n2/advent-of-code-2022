package com.empanada.solution

import cats.effect.{ExitCode, IO, IOApp}
import com.empanada.solution.Solution5.Crane.CraneSize
import com.empanada.solution.Solution5.Ship.Move
import fs2.io.file.{Files, Path}

import scala.collection.mutable

object Solution5 extends IOApp {
  def makeShip(input: String): Ship = Ship.of(input)

  final case class Crane private (value: Char)

  object Crane {
    private val delimiters = List('[', ']')
    private val CraneExample = "[X]"
    val CraneSize = CraneExample.size

    def of(base: String): Option[Crane] = {
      println(s"prev value $base")
      val cleanValue = base.filterNot(delimiters.contains)

      Option.when(cleanValue.length == 1)(
        of(cleanValue.head)
      )

    }

    def of(value: Char): Crane = Crane(value)
  }

  case class Ship private (stacks: mutable.Map[Int, List[Crane]]) {
    def top = stacks.keys.toList.sorted.foldLeft("") { case (acc, idx) =>
      val stack = stacks(idx).headOption
      if (stack.isDefined) acc + stack.get.value else acc
    }

    def col(idx: Int): List[Crane] = stacks.get(idx).toList.flatten

    def prettyPrint() = println(Prettifier.prettyFormat(stacks.toMap))

    def move(amount: Int, from: Int, to: Int): Unit = {
      for {
        currFrom <- stacks.get(from)
        currTo <- stacks.get(to)
      } yield {
        val takeFrom = currFrom.take(amount)
        val leftFrom = currFrom.drop(amount)
        stacks.update(from, leftFrom)
        stacks.update(to, takeFrom ::: currTo)
      }
    }
  }

  object Ship {
    final case class Move(amount: Int, from: Int, to: Int)

    object Move {
      def of(repr: String): Move = {
        val splitted = repr.split(" ")
        val (amount, from, to) = (splitted(1), splitted(3), splitted(5))
        Move(amount.toInt, from.toInt, to.toInt)
      }

      def multipleOf(repr: String): List[Move] =
        repr
          .split("\n")
          .filterNot(_.isEmpty)
          .map(Move.of)
          .toList
    }

    def of(str: String): Ship = {
      val lines = str
        .split("\n")
        .filterNot(_.isEmpty)
      val shipInternal = mutable.Map.empty[Int, List[Crane]]
      val lastLinePlusIndexZero = 2
      for (reverseIndex <- (lines.size - lastLinePlusIndexZero) to 0 by -1) {
        val currLine = lines(reverseIndex)
        currLine.zipWithIndex
          .collect {
            case (char, idx) if idx % 4 == 1 =>
              char
          }
          .zipWithIndex
          .foreach { case (maybeCrane, idx) =>
            val shipIndex = idx + 1
            val currStack = shipInternal.getOrElse(shipIndex, List.empty)
            if (maybeCrane.isWhitespace)
              shipInternal.update(shipIndex, currStack)
            else
              shipInternal.update(shipIndex, Crane.of(maybeCrane) :: currStack)
          }
      }
      new Ship(shipInternal)
    }
  }

  def countCranes(input: String): Int =
    _countCranes(input.split("\n").filter(_.nonEmpty).head)

  def countWhitespaces(input: String): Int =
    _countWhitespaces(input.split("\n").filter(_.nonEmpty).head)

  private def _countWhitespaces(line: String): Int = {
    _countCranes(line) - 1 // last whitespace doesn't count
  }

  private def _countCranes(line: String): Int =
    (line.size + 1) / (CraneSize + 1) // considering the last whitespace to simplify

  def parseCommands(commands: String): List[Move] =
    Move.multipleOf(commands)

  def runCommands(ship: Ship, commandsRepr: String): Ship = {
    val commands = parseCommands(commandsRepr)
    println(commands)
    commands.foreach { case Move(amount, from, to) =>
      ship.move(amount, from, to)
    }
    ship
  }

  def process(filePath: String): IO[String] =
    readInput(filePath)
      .map(s => s.trim.split("\n\n", 2))
      .map { entities =>
        val (shipRepr, commandsRepr) = (entities(0), entities(1))
        val ship = Ship.of(shipRepr)
        runCommands(ship, commandsRepr)
        ship.top
      }

  def readInput(pathStr: String): IO[String] = {
    val path = Path.apply(pathStr)
    Files
      .apply[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .compile
      .string

  }

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-5.txt"
    process(filePath)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }

  object Prettifier {

    def prettyFormat(stacks: Map[Int, List[Crane]]): String = {

      def _prettyFormat(stacks: Map[Int, List[Char]]): String = {
        val n = stacks.keys.maxOption.getOrElse(0) // total number of columns
        val maxStackHeight = stacks.values.map(_.length).maxOption.getOrElse(0)

        // Determine the width needed for each column.
        // Each crane stack takes 3 characters, plus one for separation.
        val columnWidth = 4

        val rows = (1 to maxStackHeight).reverse
          .map { level =>
            (1 to n)
              .map { col =>
                stacks.get(col) match {
                  case Some(chars) if chars.length >= level =>
                    s"[${chars(chars.length - level)}]"
                  case _ => "   " // 3 spaces when no char is present
                }
              }
              .mkString(" ") // one space between stacks
          }
          .mkString("\n")

        val footer = (1 to n).map(i => f"$i%3s").mkString(" ").trim

        rows + "\n " + footer
      }

      _prettyFormat(stacks.view.mapValues(c => c.map(_.value)).toMap)
    }

  }

}
