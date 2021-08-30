package fpdesign.solutions

import fpdesign.functors.Functor

/**
  *  To read:
  *    - https://typelevel.org/cats/typeclasses/functor.html
  *    - https://typelevel.org/cats/typeclasses/contravariant.html
  *    - scala with cats book (read 1, 2, 3 chapters)
  *    - https://www.scala-exercises.org/cats/ (functor, monoid, semigroup)
  *    - implement
  *      - map, map2, map3, map4
  */
object homework5 extends App {

  trait Applicative[F[_]] extends Functor[F] {

    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      val fab: F[A => B] = pure(f)
      ap(fab)(fa)
    }

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      val fabc: F[A => B => C] = pure(f.curried)
      val bc: F[B => C] = ap(fabc)(fa)
      ap(bc)(fb)
    }

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      val fabcd: F[A => B => C => D] = pure(f.curried)
      val fbcd = ap(fabcd)(fa)
      val fcd = ap(fbcd)(fb)
      ap(fcd)(fc)
    }

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      val fabcde = pure(f.curried)
      val fbcde = ap(fabcde)(fa)
      val fcde = ap(fbcde)(fb)
      val fde = ap(fcde)(fc)
      ap(fde)(fd)
    }

  }

}
