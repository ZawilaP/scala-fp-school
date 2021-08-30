package basics.solutions

import scala.annotation.tailrec

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
object homework2 extends App {

  sealed trait List[+A] {

    def map[B](f: A => B): List[B] = this match {
      case Cons(head, tail) => Cons(f(head), tail.map(f))
      case Nil              => Nil
    }

    def revers: List[A] = foldLeft(List.empty[A])((acc, x) => x :: acc)

    def ::[B >: A](b: B): List[B] = Cons(b, this)

    def :::[B >: A](xs: List[B]): List[B] = xs ++ this

    def ++[B >: A](xs: List[B]): List[B] = revers.foldLeft(xs)((acc, x) => x :: acc)

    def flatMap[B](f: A => List[B]): List[B] = {
      def go(xs: List[A], acc: List[B]): List[B] = xs match {
        case Cons(head, tail) => go(tail, acc ++ f(head))
        case Nil              => acc
      }
      go(this, List.empty)
    }

    def filter(f: A => Boolean): List[A] = flatMap(a => if (f(a)) List(a) else List.empty)

    def foldLeft[B](acc: B)(f: (B, A) => B): B = {
      @tailrec
      def go(xs: List[A], acc: B): B = xs match {
        case Cons(head, tail) => go(tail, f(acc, head))
        case Nil              => acc
      }
      go(this, acc)
    }

    def foldMap[B](f: A => B)(implicit m: Monoid[B]): B = map(f).foldLeft(m.empty)(m.combine)

    override def toString: String = this match {
      case xs  => "[" + xs.foldLeft("")(_ ++ "," ++ _.toString).substring(1) + "]"
      case Nil => "[]"
    }

  }

  object List {
    def empty[A]: List[A] = Nil
    def apply[A](xs: A*): List[A] = xs.reverse.foldLeft(List.empty[A])((acc, x) => x :: acc)
  }

  case class Cons[A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  sealed trait Monoid[A] {
    def combine(a: A, b: A): A
    def empty: A
  }

  val xs: List[Int] = List(1, 2, 3)
  val ys: List[Int] = List(3)

  println(xs)
  println(ys)

  println(xs ::: ys)
  println(xs ++ ys)

  val zs: List[Int] = for {
    x <- xs
    y <- ys
  } yield x + y

  assert(zs == List(4, 5, 6))

  assert(zs.filter(_ <= 5) == List(4, 5))

  val intSumMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int): Int = a + b

    def empty: Int = 0
  }

  assert(List("1", "2", "3").foldMap(_.toInt)(intSumMonoid) == 6)

  println(1 :: 2 :: 3:: 4 :: 5:: List.empty)

  println(1 :: 2 :: 3 :: List.empty ::: 1 :: 2 :: List.empty)

  val as = 1 :: 2 :: 3 :: List.empty
  val bs = 6 :: 7 :: List.empty

  val cs = 0 :: as ::: bs
  val csp = 0 :: as ++ bs

  println(cs)
  println(csp)

}
