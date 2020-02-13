package com.example.freemonadtoy

import cats._
import cats.free.Free
import cats.free.Free.liftF

object VarExample {

  object ADT {

    sealed trait Var[A]

    case class Set[T](a: T) extends Var[Unit]

    case class Get[T]() extends Var[Option[T]]

  }

  object Ops {

    import ADT._

    def set[T](a: T): Free[Var, Unit] = liftF[Var, Unit](Set(a))

    def get[T]: Free[Var, Option[T]] = liftF[Var, Option[T]](Get[T]())
  }

  object Interpreters {

    import ADT._

    def simpleInterpreter: Var ~> Id = new (Var ~> Id) {
      var v: Option[Any] = None

      override def apply[A](varA: Var[A]): Id[A] = varA match {
        case Set(a) => v = Some(a)
        case Get()  => v.map(_.asInstanceOf[A])
      }
    }

    def historyInterpreter: Var ~> List = new (Var ~> List) {
      var p = List.empty[Any]

      override def apply[A](varA: Var[A]): List[A] = varA match {
        case Set(a) =>
          p = a :: p
          List(())
        case Get() =>
          p.map(x => Option(x.asInstanceOf[A]))
      }
    }
  }

  object Main {
    import Ops._
    import ADT._
    import cats.syntax.apply._
    import cats.instances.option._
    import cats.instances.list._

    val program: Free[Var, Option[Int]] = for {
      _ <- set(1)
      optX <- get[Int]
      _ <- set(2)
      optY <- get[Int]
    } yield {
      (optX, optY).tupled.map { case (x, y) => x + y }
    }

    val program2: Free[Var, Option[Int]] = for {
      x <- get[Int]
      _ <- set(42)
    } yield x

    val programWithIdResults = program.foldMap(Interpreters.simpleInterpreter)
    val programWithListResults =
      program.foldMap(Interpreters.historyInterpreter)
    val program2WithIdResults = program2.foldMap(Interpreters.simpleInterpreter)
    val program2WithListResults =
      program2.foldMap(Interpreters.historyInterpreter)

    def run() = {
      println(s"Program with Id: $programWithIdResults")
      println(s"Program with List: $programWithListResults")
      println(s"Program 2 with Id: $program2WithIdResults")
      println(s"Program 2 with List: $program2WithListResults")
    }
  }
}
