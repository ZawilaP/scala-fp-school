package fpdesign.solutions

import basics.homework2.Monoid
import fpdesign.functors.Functor
import fpdesign.functors.Functor._
import fpdesign.functors.Functor.instances._

/**
  *  To Read:
  *    - https://en.wikipedia.org/wiki/Type_constructor
  *    - https://en.wikipedia.org/wiki/Bottom_type
  *    - https://en.wikipedia.org/wiki/Type_theory
  *    - https://github.com/typelevel/kind-projector
  *    - https://en.wikipedia.org/wiki/Functor
  *    - https://en.wikipedia.org/wiki/Natural_transformation
  *    - https://en.wikipedia.org/wiki/Endomorphism
  *
  *  Try to implement:
  *    - functor instance for
  *      - Read
  *      - Show
  *      - Either
  *      - Hash
  *    - what are your observations (if any) ?
  *    - implement parse
  *      - is it safe ?
  *    - implement foldMap, Foldable, intMonoidInstance, vectorFolableInstance, vectorFunctorInstance to assert a given expression
  *    - implement distribute and codistribute
  */
object homework4 extends App {

  trait Read[A] {
    def read(s: String): A
  }

  object Read {
    implicit val functorInstance: Functor[Read] = new Functor[Read] {
      def map[A, B](fa: Read[A])(f: A => B): Read[B] = (s: String) => f(fa.read(s))
    }
  }

  trait Show[A] {
    def show(a: A): String
  }

  // no covariant functor instance for show

  implicit val stringEitherFunctorInstance: Functor[Either[String, *]] = new Functor[Either[String, *]] {
    def map[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa.map(f)
  }

  trait Hash[A] {
    def hash(x: A): Int
  }

  // no covariant functor instance for hash

  def parse[F[_]: Functor](xs: F[String]): F[Int] = xs.fmap(_.toInt)

  implicit val setFunctorInstance: Functor[Set] = new Functor[Set] {
    def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
  }

  assert(parse(Set("1", "2", "3")) == Set(1, 2, 3))

  trait Foldable[F[_]] {
    def foldLef[A](fa: F[A])(m: Monoid[A]): A
  }

  def foldMap[F[_]: Functor: Foldable, A, B: Monoid](fa: F[A])(f: A => B): B =
    implicitly[Foldable[F]].foldLef(fa.fmap(f))(implicitly[Monoid[B]])

  case class Money(value: Int)

  implicit val intMonoidInstance: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int): Int = a + b
    def empty: Int = 0
  }
  implicit val vectorFunctorInstance: Functor[Vector] = new Functor[Vector] {
    def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }
  implicit val vectorFolableInstance: Foldable[Vector] = new Foldable[Vector] {
    def foldLef[A](fa: Vector[A])(m: Monoid[A]): A = fa.foldLeft(m.empty)(m.combine)
  }

  assert(foldMap(Vector(Money(1), Money(2), Money(3)))(_.value) == 6)

  def distribute[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (fab.fmap { case (a, _) => a }, fab.fmap { case (_, b) => b })

  def codistribute[F[_]: Functor, A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(value)  => value.fmap(Left.apply)
      case Right(value) => value.fmap(Right.apply)
    }

  assert(distribute(List("one" -> 1, "two" -> 2, "three" -> 3)) == (List("one", "two", "three"), List(1, 2, 3)))
  assert(codistribute(Right(List(1, 2, 3))) == List(Right(1), Right(2), Right(3)))
  assert(codistribute(Left(List("one", "two"))) == List(Left("one"), Left("two")))
}
