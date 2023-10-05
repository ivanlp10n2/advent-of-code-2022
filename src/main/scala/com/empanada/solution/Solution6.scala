package com.empanada.solution

import cats.effect.kernel.Ref
import cats.effect.{Deferred, ExitCode, IO, IOApp}
import fs2.{Chunk, Collector}
import fs2.concurrent.{Signal, SignallingRef}
import fs2.io.file.{Files, Path}

import scala.collection.mutable

object Solution6 extends IOApp {

  def hasRepeatedChars(value: String): Boolean = {
    val founded = mutable.Map.empty[Char, Unit]
    for (idx <- 0 until value.size) {
      if (founded.get(value(idx)).isEmpty)
        founded.update(value(idx), ())
      else
        return true
    }
    false
  }

  def findStartOfPackage(
      signal: SignallingRef[IO, Boolean]
  ): fs2.Stream[IO, Chunk[Char]] => fs2.Stream[IO, Unit] =
    input => {
      input
        .map { chunk => chunk.toList.mkString("") }
        .evalTap { str =>
          if (!hasRepeatedChars(str)) signal.set(true) else IO.pure(str)
        }
        .evalMap(str =>
          signal.get
            .flatTap(ref =>
              if (ref) IO.println(s"previous String was unique $str")
              else IO.unit
            )
            .as(())
        )

    }

  def process(filePath: String): IO[Int] = {
    SignallingRef[IO, Boolean](false).flatMap { foundStart =>
      IO.ref(0).flatMap { counter =>
        readInput(filePath)
          .flatMap(str => fs2.Stream(str: _*))
          .evalTap(_ => counter.update(_ + 1))
          .sliding(14)
          .through(findStartOfPackage(foundStart))
          .interruptWhen(foundStart)
          .evalTap(_ =>
            counter.get.flatMap(n => IO.println(s"counter: ${n}"))
          )
          .evalMap(_ => counter.get)
          .compile
          .onlyOrError
      }

    }
  }

  def readInput(pathStr: String): fs2.Stream[IO, String] = {
    val path = Path.apply(pathStr)
    Files
      .apply[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
  }
  override def run(args: List[String]): IO[ExitCode] = {
    val filePath = "./src/main/resources/input-6.txt"
    process(filePath)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }

}
