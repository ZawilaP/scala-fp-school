package hardingcats

import cats._
import cats.data._
import cats.implicits._

import java.time.LocalDateTime
import concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Await, Future}

/**
  *   - Id monad
  *   - NonEmptyList
  *   - Chain
  *     - NonEmptyChain
  *   - Validated
  *     - ValidatedNel
  *   - Writer monad
  *   - State monad
  *   - Reader monad
  *   - Kleisli arrow
  */
object dataclasses extends App {

  implicit class FutureOps[A](val fa: Future[A]) extends AnyVal {
    def get: A = Await.result(fa, Duration.Inf)
  }

  // Id monad

  val a: Id[Int] = 1
  val b: Id[Int] = 2
  println(a)
  println(b)
  println(a + b)

  def addInt(a: Int, b: Int): Int = a + b

  def add[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] =
    for {
      a <- fa
      b <- fb
    } yield addInt(a, b)

  println("add: " + add(a, b))
  println("add: " + add(1.some, 2.some))
  println("add: " + add(1.asRight[String], 2.asRight[String]))
  println("add: " + add(1.some, none[Int]))
  println("add: " + add(Future(1), Future(2)).get)

  // NonEmptyList
  val xs: NonEmptyList[Int] = NonEmptyList.of(1, 2, 3, 4)
  println(xs.head)
  val ys: List[Int] = xs.tail

  // List appending in O(n), prepending O(1)
  // Chain appending + prepend in O(1)
  val zs: Chain[Int] = Chain.one(1) ++ Chain.one(2) ++ Chain.one(2)
  println(zs)
  println(zs.toList)

  def measure[A](a: => A): A = {
    val start = System.currentTimeMillis()
    val memo = a
    val duration = System.currentTimeMillis() - start
    println("duration: " + duration)
    memo
  }

  val n = Int.MaxValue / 1000
  val us = List.fill(n)(1)
  measure(us ++ us)
  measure(Chain.fromSeq(us) ++ Chain.fromSeq(us))

  // NonEmptyChain

  NonEmptyChain.one(1).head

  // Validated

  case class User(age: Int, name: String)

  def validAge: Validated[List[String], Int] = Validated.valid(13)
  def inValidAge: Validated[List[String], Int] = Validated.invalid(List("age is too low"))
  def validName: Validated[List[String], String] = Validated.valid("Ron")
  def inValidName: Validated[List[String], String] = Validated.invalid(List("name is to short"))

  println((validAge, validName).mapN(User.apply))
  println((inValidAge, validName).mapN(User.apply))
  println((inValidAge, inValidName).mapN(User.apply))

  // Writer monad

  type Logged[A] = Writer[List[String], A]

  def add2(a: Int, b: Int): Logged[Int] = Writer(List(s"$a + $b"), a + b)
  def add3(a: Int, b: Int, debug: Boolean): Logged[Int] =
    if (debug) Writer(List(s"$a + $b"), a + b) else Writer(List.empty, a + b)
  def mult2(a: Int, b: Int): Logged[Int] = Writer(List(s"$a * $b"), a * b)
  def doSomething(a: Int, b: Int): Logged[Int] =
    for {
      aa <- add2(a, b)
      _ <- Writer.tell(List("date: " + LocalDateTime.now))
      bb <- mult2(aa, b)
    } yield bb

  val (logg, value) = doSomething(1, 2).run

  println("value: " + value)

  // State monad

  private var seq: Int = 1

  def nextIntImp: Int = {
    val memo = seq
    seq = seq + 1
    memo
  }

  println("ids: " + List.fill(3)(nextIntImp))

  def nextInt(s: Int): (Int, Int) = (s + 1, s)

  val (seq1, id1) = nextInt(1)
  val (seq2, id2) = nextInt(seq1)
  val (seq3, id3) = nextInt(seq2)

  println("ids: " + List(id1, id2, id3))

  type IntState[A] = State[Int, A]

  val nexIntS: IntState[Int] =
    for {
      seq <- State.get[Int]
      _ <- State.set(seq + 1)
    } yield seq

  val ids: IntState[List[Int]] = List.fill(5)(nexIntS).sequence

  println("ids: " + ids.runA(1).value)

}
