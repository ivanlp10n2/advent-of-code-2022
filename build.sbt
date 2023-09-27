ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.5"

lazy val root = (project in file(".")).settings(
  name := "advent-of-code-2022",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    // I/O library
    "co.fs2" %% "fs2-core" % "3.9.2",
    "co.fs2" %% "fs2-io" % "3.9.2",
    // better monadic for compiler plugin as suggested by documentation
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

    //test
    "org.scalatest" %% "scalatest" % "3.2.17" % "test",
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.4.0" % Test
  )
)
