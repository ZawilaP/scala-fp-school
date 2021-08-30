package fpdesign

import basics.homework2.Monoid

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

  trait Show[A] {
    def show(a: A): String
  }

  trait Hash[A] {
    def hash(x: A): Int
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]

    implicit class FunctorOps[A, F[A]](val fa: F[A]) extends AnyVal {
      def fmap[B](f: A => B)(implicit ev: Functor[F]): F[B] = ev.map(fa)(f)
    }

    object instances {
      type ThrowEither[A] = Either[Throwable, A] // * -> *

      implicit val eitherInstance: Functor[ThrowEither] = new Functor[ThrowEither] {
        def map[A, B](fa: ThrowEither[A])(f: A => B): ThrowEither[B] = fa.map(f)
      }

      implicit val readInstance: Functor[Read] = new Functor[Read] {
        def map[A, B](fa: Read[A])(f: A => B): Read[B] = (s: String) => f(fa.read(s))
      }

//      implicit val showInstance: Functor[Show] = new Functor[Show] {
//        def show[A](f: A => String): Show[A] = (a: A) => f(a)
//
//        def map[A, B](fa: Show[A])(f: A => B): Show[B] = new Show[B] {
//          def show[A](g: B => String): Show[B] = fa.fmap(f)
//        }
//      }
//
//      implicit val hashInstance: Functor[Hash] = new Functor[Hash] {
//        def map[A, B](fa: Hash[A])(f: A => B): Hash[B] = new Hash[B] {
//          def hash(x: B): Int = fa.fmap(f
//        }
//      }
    }
  }

  import Functor._

  def parse[F[_]: Functor](xs: F[String]): F[Int] = xs.fmap(x => x.toInt)

  implicit val setFunctorInstance: Functor[Set] = new Functor[Set] {
    def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
   }

  assert(parse(Set("1", "2", "3")) == Set(1, 2, 3))

  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], acc: B)(f: (B, A) => B): B
  }

  def foldMap[F[_]: Functor: Foldable, A, B: Monoid](fa: F[A])(f: A => B): B =

  case class Money(value: Int)
  implicit val intMonoidInstance: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int): Int = a + b
    def empty: Int = 0
  }
  implicit val vectorFunctorInstance: Functor[Vector] = ???
  implicit val vectorFolableInstance: Foldable[Vector] = ???

  assert(foldMap(Vector(Money(1), Money(2), Money(3)))(_.value) == 6)

  def distribute[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = ???

  def codistribute[F[_]: Functor, A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = ???

  assert(distribute(List("one" -> 1, "two" -> 2, "three" -> 3)) == (List("one", "two", "three"), List(1, 2, 3)))
  assert(codistribute(Right(List(1, 2, 3))) == List(Right(1), Right(2), Right(3)))
  assert(codistribute(Left(List("one", "two"))) == List(Left("one"), Left("two")))

}
