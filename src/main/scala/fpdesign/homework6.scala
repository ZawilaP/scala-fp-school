package fpdesign

import fpdesign.applicative.Applicative

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
  *      - Validated.ap
  */
object homework6 extends App {

  implicit val applicativeListInstance: Applicative[List] = ???

  implicit val applicativeEitherInstance: Applicative[Either[String, *]] = ???

  implicit val applicativeSetInstance: Applicative[Set] = ???

  trait SemiGroup[A] {
    def combine(a: A, b: A): A
  }

  sealed trait Validated[+E, +A] {
    def ap[EE >: E, B](ff: Validated[EE, A => B])(implicit EE: SemiGroup[EE]): Validated[EE, B] = ???
  }

  object Validated {

    case class Valid[A](value: A) extends Validated[Nothing, A]

    case class InValid[E](e: E) extends Validated[E, Nothing]

  }

}
