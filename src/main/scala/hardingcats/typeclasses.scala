package hardingcats

import cats._
import cats.implicits._

/**
  *   - Functor
  *   - Applicative
  *   - Monad
  *   - Semigroup
  *   - Monoid
  *   - Traverse
  *   - Foldable
  *   - Eq
  *   - Show
  *   - Eval
  *     - later  (lazy val)
  *     - now    (val)
  *     - always (def)
  */
object typeclasses extends App {

  // Functor

  println(Functor[List].map(List(1, 2, 3))(_ + 1))

  // Applicative

  println(Applicative[List].map2(List(1, 2, 3), List(1, 2))(_ + _))

  case class Foo(a: Int, s: String)

  println((1.some, "message".some).mapN(Foo.apply))

  // Monad

  println(Monad[List].pure(1))
  println(Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)))

  // Semigroup

  println(123.some |+| 34.some)

  // Monoid

  println(Monoid[List[String]].empty)

  // Traverse

  println(Traverse[List].sequence(List(1.some, 3.some)))
  println(List(1.some, 3.some).sequence)

  // Foldable

  println("Foldable")
  val m: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }
  println(Foldable[List].fold(List(1, 2, 3, 4, 5))(m))

  // Eval

  val now: Eval[Int] = Eval.now {
    println("eval now")
    1
  }

  println("value: " + now.value)
  println("value: " + now.value)

  val always: Eval[Int] = Eval.always {
    println("eval always")
    2
  }

  println("value: " + always.value)
  println("value: " + always.value)

  val later: Eval[Int] = Eval.later {
    println("eval later")
    3
  }

  println("value: " + later.value)
  println("value: " + later.value)

  val memo = always.map(_ + 10).memoize
  println("memo value: " + memo.value)
  println("memo value: " + memo.value)

//  def fact(n: Long): Eval[Long] = Eval.always {
//    if (n == 0) Eval.now(1)
//    else fact(n - 1).map(_ * n)
//  }
//
//  println(fact(1).value)
//  println(fact(4).value)
//  println(fact(100000000000L).value)

}
