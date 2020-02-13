package com.example.freemonadtoy

import cats.data.EitherK
import cats.{Id, InjectK, ~>}
import cats.effect.IO
import cats.free.Free

object CompositionExample {
  object VarADT {
    sealed trait Var[A]
    case class Set[T](a: T) extends Var[Unit]
    case class Get[T]() extends Var[Option[T]]
  }

  import VarADT._
  class VarOps[C[_]](implicit inject: InjectK[Var, C]) {
    def set[T](a: T): Free[C, Unit] = Free.inject[Var, C](Set(a))
    def get[T]: Free[C, Option[T]] = Free.inject[Var, C](Get[T]())
  }

  object VarOps {
    implicit def varOps[F[_]](implicit I: InjectK[Var, F]): VarOps[F] =
      new VarOps[F]
  }

  object ConsoleADT {
    trait Console[A]
    case class ReadLine() extends Console[String]
    case class PrintLine(s: String) extends Console[Unit]

  }

  import ConsoleADT._
  class ConsoleOps[C[_]](implicit inject: InjectK[Console, C]) {
    def readLine: Free[C, String] = Free.inject[Console, C](ReadLine())
    def printLine(s: String): Free[C, Unit] =
      Free.inject[Console, C](PrintLine(s))
  }

  object ConsoleOps {
    implicit def consoleOps[F[_]](
      implicit I: InjectK[Console, F]
    ): ConsoleOps[F] = new ConsoleOps[F]
  }

  object Interpreters {
    def consoleInterpreter = new (Console ~> IO) {
      def apply[A](c: Console[A]): IO[A] = c match {
        case ReadLine()           => IO(scala.io.StdIn.readLine())
        case PrintLine(s: String) => IO(println(s))
      }
    }

    def simpleInterpreter = new (Var ~> IO) {
      var v: Option[Any] = None

      override def apply[A](varA: Var[A]): IO[A] = varA match {
        case Set(a) =>
          IO {
            v = Some(a)
          }
        case Get() => IO(v.map(_.asInstanceOf[A]))
      }
    }

    def simpleAppInterpreter = simpleInterpreter or consoleInterpreter
  }

  object Main {
    type App[A] = EitherK[Var, Console, A]

    def program(implicit V: VarOps[App],
                C: ConsoleOps[App]): Free[App, String] = {
      import V._
      import C._

      for {
        _ <- set("")
        firstLine <- readLine
        v <- get[String]
        _ <- set(v.get + firstLine)
        secondLine <- readLine
        v2 <- get[String]
        _ <- set(v2.get + secondLine)
        v3 <- get[String]
      } yield v3.get
    }

    def run() = {
      import ConsoleOps._
      import VarOps._
      import Interpreters.simpleAppInterpreter

      val programResult = program.foldMap(simpleAppInterpreter)
      programResult.unsafeRunSync()
    }
  }
}
