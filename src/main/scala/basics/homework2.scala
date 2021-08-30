package basics

/**
  * Read:
  *   - https://docs.scala-lang.org/overviews/core/value-classes.html
  *
  * Watch:
  *   - https://www.youtube.com/watch?v=V10hzjgoklA&list=PLw1ByaFQI-9ICX6a6T4f2OW-w9SV17MUg&index=1&t=1609s&ab_channel=LambdaWorld
  *
  * Implement
  *   - functions:  flatMap, filter, foldLeft, foldMap, List.apply
  *   - monoid instance
  */
object homework2 {

  sealed trait List[+A] {

    def map[B](f: A => B): List[B] = this match {
      case Cons(head, tail) => Cons(f(head), tail.map(f))
      case Nil              => Nil
    }

    def flatMap[B](f: A => List[B]): List[B] = ???
    def filter(f: A => Boolean): List[A] = ???
    def foldLeft[B](acc: B)(f: (B, A) => B): B = ???
    def foldMap[B](f: A => B)(implicit m: Monoid[B]): B = ???

  }

  object List {
    def empty[A]: List[A] = Nil
    def apply[A](xs: A*): List[A] = ???
  }

  case class Cons[A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  trait Monoid[A] {
    def combine(a: A, b: A): A
    def empty: A
  }

  val xs: List[Int] = List(1, 2, 3)
  val ys: List[Int] = List(3)

  val zs: List[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  assert(zs == List(4, 5, 6))

  val intSumMonoid: Monoid[Int] = ???

  assert(List("1", "2", "3").foldMap(_.toInt)(intSumMonoid) == 6)

}
