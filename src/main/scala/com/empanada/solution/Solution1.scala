package com.empanada.solution

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Chunk
import fs2.io.file.{Files, Path}

object Solution1 extends IOApp {

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
      .split(_.isEmpty)
      .map(sumStrChunk)
      .fold(0)(highest)
      .compile
      .onlyOrError
  }

  private def sumStrChunk: Chunk[String] => Int =
    (chunk: Chunk[String]) =>
      chunk.map(_.toInt).foldLeft(0)(_ + _)

  private def highest: (Int, Int) => Int = (base, current) =>
    if (base < current) current
    else base

  def run(filepath: String): IO[Int] = {
    val input = readInput(filepath)
    process(input)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-1.txt"
    run(filePath).as(ExitCode.Success)
  }
}
