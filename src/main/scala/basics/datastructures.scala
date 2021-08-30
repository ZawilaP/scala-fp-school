package basics

import scala.util.Try

/**
  *   - algebraic data type (product, sum)
  *     - Maybe (Option)
  *     - List
  */
object datastructures extends App {

  sealed trait Maybe[+A] {
    def map[B](f: A => B): Maybe[B] = this match {
      case Just(a) => Just(f(a))
      case None    => None
    }

    def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
      case Just(a) => f(a)
      case None    => None
    }

  }

  object Maybe {
    def pure[A](a: A): Maybe[A] = if (a == null) None else Just(a)
    def fromOption[A](o: Option[A]): Maybe[A] = o match {
      case Some(value) => Just(value)
      case scala.None  => None
    }
  }

  implicit class OptionOpts[A](val a: Option[A]) extends AnyVal {
    def toMaybe: Maybe[A] = Maybe.fromOption(a)
  }

  case class Just[A](a: A) extends Maybe[A]

  case object None extends Maybe[Nothing]

  val x: Maybe[Int] = Just(1)
  val y: Maybe[Int] = None

  println(x)
  println(x.map(_ + 2))
  println(y.map(_ + 2))

  def add(a: String, b: String): Maybe[Int] =
    for {
      fa <- Maybe.fromOption(Try(a.toInt).toOption)
      fb <- Maybe.fromOption(Try(b.toInt).toOption)
    } yield fa + fb

  println(add("1", "2"))
  println(add("a", "2"))

  def add2(a: String, b: String): Maybe[Int] =
    for {
      fa <- Try(a.toInt).toOption.toMaybe
      fb <- Try(b.toInt).toOption.toMaybe
    } yield fa + fb

  println("add2: " + add2("1", "2"))

  def div(a: Maybe[Int], b: Int): Maybe[Int] =
    a.flatMap { x =>
      if (b != 0) Maybe.pure(x / b) else None
    }

  sealed trait List[+A] {

    def map[B](f: A => B): List[B] = this match {
      case Cons(head, tail) => Cons(f(head), tail.map(f))
      case Nil              => Nil
    }

    def flatMap[B](f: A => List[B]): List[B] = ???
    def filter(f: A => Boolean): List[A] = ???
    def foldLeft[B](acc: B)(f: (B, A) => B): B = ???

  }

  object List {
    def empty[A]: List[A] = Nil
  }

  case class Cons[A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  val xs: List[Int] = Cons(1, Cons(2, Cons(3, List.empty)))

  println(xs)
  println(xs.map(_ + 1))

}
