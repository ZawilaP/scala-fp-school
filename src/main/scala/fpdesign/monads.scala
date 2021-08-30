package fpdesign

import fpdesign.applicative.Applicative

import scala.concurrent.{Await, Future}
import scala.util.Try
import concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration

/**
  *  - monad type class
  *    - primitives: pure, flatMap
  *    - primitives: pure, map, join (def join[A](mma: F[F[A]]): F[A])
  *  - for comprehension for monads
  *  - example of monads
  *    - List
  *    - Option
  *    - Future
  *  - monads vs applicative
  *  - 'effects' in fp
  *  - laws
  *    - associative      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
  *    - left identity    flatMap(x)(pure) == x
  *    - right identity   flatMap(unit(y))(f) == f(y)
  *  - some good to know monads
  *    - Id monad
  *    - StateMonad
  *    - WriterMonad
  *    - ReaderMonad
  *  - composing monads
  *    - monads transformers
  *  - Kleisly arrows
  */
object monads extends App {

  // * -> *
  trait Monad[F[_]] extends Applicative[F] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] = flatMap(fab) { f =>
      flatMap(fa) { a =>
        pure(f(a))
      }
    }
  }

  object Monad {
    def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

    implicit class MonadOps[A, F[_]](val fa: F[A]) extends AnyVal {

      def map[B](f: A => B)(implicit F: Monad[F]): F[B] = F.map(fa)(f)

      def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

    }

    implicit val optionInstance: Monad[Option] = new Monad[Option] {

      def pure[A](a: A): Option[A] = Option(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    }

    implicit val listInstance: Monad[List] = new Monad[List] {

      def pure[A](a: A): List[A] = List(a)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    }

    implicit val futureInstance: Monad[Future] = new Monad[Future] {

      def pure[A](a: A): Future[A] = Future.successful(a)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

    }

  }

  trait Monad2[F[_]] {
    def pure[A]: F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def join[A](ffa: F[F[A]]): F[A] // alias flatten

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  }

  trait SemiGroup[A] {
    def combine(a: A, b: A): A
  }

  object SemiGroup {
    def apply[A: SemiGroup]: SemiGroup[A] = implicitly[SemiGroup[A]]

    implicit val intSumSemi: SemiGroup[Int] = new SemiGroup[Int] {
      def combine(a: Int, b: Int): Int = a + b
    }

  }

  import Monad._
  def operation[F[_]: Monad, A, B: SemiGroup](fa: F[A], fb: F[A])(f: A => F[B]): F[B] =
    for {
      a <- fa.flatMap(f)
      b <- fb.flatMap(f)
    } yield SemiGroup[B].combine(a, b)

  def parse1(s: String): Option[Int] = Try(s.toInt).toOption
  def parse2 = (parse1 _).andThen(_.toList)
  def parse3(s : String) : Future[Int] = parse1(s) match {
    case Some(value) => Future.successful(value)
    case None =>Future.failed(new RuntimeException(s"Error during parsing $s"))
  }

  println(operation(Option("1"), Option("2"))(parse1))
  println(operation(List("1"), List("2", "3"))(parse2))
  println(Await.result(operation(Future("1"), Future("2"))(parse3), Duration.Inf))

  println(operation(List("1a"), List("2", "3"))(parse2))
  println(operation(Option("a"), Option("2"))(parse1))
  println(operation(None, Option("2"))(parse1))
}
