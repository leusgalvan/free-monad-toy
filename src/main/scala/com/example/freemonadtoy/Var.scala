package com.example.freemonadtoy

import cats._
import cats.free.Free
import cats.free.Free.liftF

object ADT {
  sealed trait VarA[A]
  case class Set[T](a: T) extends VarA[Unit]
  case class Get[T]() extends VarA[Option[T]]
}

object Ops {
  import ADT._

  type Var[A] = Free[VarA, A]

  def set[T](a: T): Var[Unit] = liftF[VarA, Unit](Set(a))
  def get[T]: Var[Option[T]] = liftF[VarA, Option[T]](Get[T]())
}

object Compilers {
  import ADT._

  def simpleCompiler: VarA ~> Id = new (VarA ~> Id) {
    var v: Option[Any] = None

    override def apply[A](varA: VarA[A]): Id[A] = varA match {
      case Set(a) =>
        v = Some(a)
        ()
      case Get() => v.map(_.asInstanceOf[A])
    }
  }

  def listCompiler: VarA ~> List = new (VarA ~> List) {
    var p = List.empty[Any]

    override def apply[A](varA: VarA[A]): List[A] = varA match {
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
  import cats.syntax.apply._
  import cats.instances.option._
  import cats.instances.list._

  val program: Var[Option[Int]] = for {
    _ <- set(1)
    optX <- get[Int]
    _ <- set(2)
    optY <- get[Int]
  } yield {
    (optX, optY).tupled.map { case (x, y) => x + y }
  }

  val program2: Var[Option[Int]] = for {
    x <- get[Int]
    _ <- set(42)
  } yield x

  val programWithIdResults = program.foldMap(Compilers.simpleCompiler)
  val programWithListResults = program.foldMap(Compilers.listCompiler)
  val program2WithIdResults = program2.foldMap(Compilers.simpleCompiler)
  val program2WithListResults = program2.foldMap(Compilers.listCompiler)

  def run() = {
    println(s"Program with Id: $programWithIdResults")
    println(s"Program with List: $programWithListResults")
    println(s"Program 2 with Id: $program2WithIdResults")
    println(s"Program 2 with List: $program2WithListResults")
  }
}