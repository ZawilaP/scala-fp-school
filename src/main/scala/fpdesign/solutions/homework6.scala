package fpdesign.solutions

import fpdesign.applicative.Applicative
import fpdesign.solutions.homework6.Validated.{InValid, Valid}

/**
  *  To read:
  *    - https://typelevel.org/cats/typeclasses/applicative.html
  *    - scala with cats book (read 6 chapters)
  *    - https://www.scala-exercises.org/cats/ (applicative)
  *    - http://learnyouahaskell.com/chapters
  *    - https://eed3si9n.com/herding-cats/
  *    - https://eed3si9n.com/learning-scalaz/
  *    - https://zio.dev/ (optional)
  *    - implement
  *      - applicative instances for List, Either and Set
  *       - Validated.ap
  */
object homework6 extends App {

  implicit val applicativeListInstance: Applicative[List] = new Applicative[List] {
    def ap[A, B](fab: List[A => B])(fa: List[A]): List[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)

    def pure[A](a: A): List[A] = List(a)
  }

  implicit val applicativeEitherInstance: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    def ap[A, B](fab: Either[String, A => B])(fa: Either[String, A]): Either[String, B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)

    def pure[A](a: A): Either[String, A] = Right(a)
  }

  implicit val applicativeSetInstance: Applicative[Set] = new Applicative[Set] {
    def ap[A, B](fab: Set[A => B])(fa: Set[A]): Set[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)

    def pure[A](a: A): Set[A] = Set(a)
  }

  trait SemiGroup[A] {
    def combine(a: A, b: A): A
  }

  sealed trait Validated[+E, +A] {
    def ap[EE >: E, B](ff: Validated[EE, A => B])(implicit EE: SemiGroup[EE]): Validated[EE, B] = (this, ff) match {
      case (Valid(a), Valid(f))         => Valid(f(a))
      case (InValid(a), InValid(b))     => InValid(EE.combine(a, b))
      case (Valid(_), err @ InValid(_)) => err
      case (err @ InValid(_), _)        => err
    }
  }

  object Validated {

    case class Valid[A](value: A) extends Validated[Nothing, A]

    case class InValid[E](e: E) extends Validated[E, Nothing]

  }

}
