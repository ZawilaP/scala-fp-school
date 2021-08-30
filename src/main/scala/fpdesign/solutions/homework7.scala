package fpdesign.solutions

import fpdesign.functors.Functor

import scala.util.Try

/**
  *  To read:
  *    - Read  about Donald Knuth (https://en.wikipedia.org/wiki/Donald_Knuth)
  *       - https://en.wikipedia.org/wiki/MMIX
  *     - https://typelevel.org/cats/
  *       - https://typelevel.org/cats/typeclasses.html
  *       - https://typelevel.org/cats/datatypes.html
  *       - https://typelevel.org/cats/resources_for_learners.html
  *       - https://typelevel.org/cats/guidelines.html
  *     - https://typelevel.org/cats-effect/
  *       - https://typelevel.org/cats-effect/tutorial/tutorial.html
  *       - https://typelevel.org/cats-effect/concurrency/basics.html
  *       - https://typelevel.org/cats-effect/concurrency/
  *
  *  Implement
  *      - def replicateM[A](n: Int, fa: F[A]): F[List[A]]
  *       - def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]]
  */
object homework7 extends App {

  trait Applicative[F[_]] extends Functor[F] {

    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      val fab: F[A => B] = pure(f)
      ap(fab)(fa)
    }

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      val cf: A => B => C = f.curried
      val fabc: F[A => B => C] = pure(cf)
      val fbc: F[B => C] = ap(fabc)(fa)
      val fc: F[C] = ap(fbc)(fb)
      fc
    }

    def sequence[A](xs: scala.List[F[A]]): F[scala.List[A]] =
      map(xs.foldLeft(pure(List.empty[A])) { (acc, fa) =>
        map2(fa, acc)(_ :: _)
      })(_.reverse)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(scala.List.fill(n)(fa))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      map(ms.foldLeft(pure(scala.List.empty[A])) { (acc, a) =>
        map2(acc, f(a)) {
          case (acc2, true)  => a :: acc2
          case (acc2, false) => acc2
        }
      })(_.reverse)

  }

  object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

    implicit lazy val optionInstance: Applicative[Option] = new Applicative[Option] {

      def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
        for {
          f <- fab
          a <- fa
        } yield f(a)

      def pure[A](a: A): Option[A] = Option(a)
    }

    implicit val applicativeListInstance: Applicative[List] = new Applicative[List] {
      def ap[A, B](fab: List[A => B])(fa: List[A]): List[B] =
        for {
          f <- fab
          a <- fa
        } yield f(a)

      def pure[A](a: A): List[A] = List(a)
    }

  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  assert(Applicative[Option].replicateM(5, Some(1)) == Some(List(1, 1, 1, 1, 1)))
  assert(Applicative[Option].replicateM(5, None) == None)

  def even(s: String): Option[Boolean] = parseInt(s).map(_ % 2 == 0)

  assert(Applicative[Option].filterM(List("1", "2", "3", "4"))(even) == Some(List("2", "4")))
  assert(Applicative[Option].filterM(List("1", "2a", "3", "4"))(even) == None)

}
