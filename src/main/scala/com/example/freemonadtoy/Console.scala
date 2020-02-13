package com.example.freemonadtoy

import java.io.{BufferedWriter, FileWriter}

import cats.effect.{IO, Resource}
import cats.free.Free
import cats.free.Free.liftF
import cats.~>

object ConsoleExample {

  object ADT {

    trait Console[A]

    case class ReadLine() extends Console[String]

    case class PrintLine(line: String) extends Console[Unit]
  }

  object Ops {

    import ADT._

    def readLine: Free[Console, String] = liftF[Console, String](ReadLine())

    def printLine(line: String): Free[Console, Unit] =
      liftF[Console, Unit](PrintLine(line))
  }

  object Interpreters {

    import ADT._

    val consoleInterpreter = new (Console ~> IO) {
      def apply[A](c: Console[A]): IO[A] = c match {
        case ReadLine()              => IO(scala.io.StdIn.readLine())
        case PrintLine(line: String) => IO(println(line))
      }
    }

    val printToFileInterpreter = new (Console ~> IO) {
      val filename = "out.txt"
      val fileWriter = Resource.fromAutoCloseable(
        IO(new BufferedWriter(new FileWriter(filename, true)))
      )

      def apply[A](c: Console[A]): IO[A] = c match {
        case ReadLine() => IO(scala.io.StdIn.readLine())
        case PrintLine(s) =>
          fileWriter.use(
            writer =>
              IO {
                writer.append(s"$s\n")
                writer.flush()
            }
          )
      }
    }
  }

  object Main {

    import ADT._
    import Ops._
    import Interpreters._

    val echo: Free[Console, Unit] = for {
      line <- readLine
      _ <- printLine(line)
      _ <- echo
    } yield ()

    val echoConsoleResults: IO[Unit] = echo.foldMap(printToFileInterpreter)

    def run() = {
      echoConsoleResults.unsafeRunSync()
    }
  }
}
