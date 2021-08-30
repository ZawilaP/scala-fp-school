package fpdesign

import fpdesign.applicative.Validated.{InValid, Valid}
import fpdesign.functors.Functor

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
  *  - applicative type class
  *  - example of applicative instance
  *    - option
  *  - validated data type
  *  - validated applicative instance
  *  - validated vs either
  *   - applicative combinators
  *     - def sequence[A](xs: List[F[A]]): F[List[A]]
  *     - def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]]
  *     - def replicateM[A](n: Int, fa: F[A]): F[List[A]]
  *     - def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)]
  *     - def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]]
  *  - laws
  *    - functor laws (identity, composition)
  *    - left identity   map2(unit(()), fa)((_,a) => a) == fa
  *    - right identity  map2(fa, unit(()))((a,_) => a) == fa
  *    - associativity   product(product(fa,fb),fc) == map(product(fa, product(fb,fc)))(assoc)
  */
object applicative extends App {

  // * -> *
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

    def sequence[A](xs: List[F[A]]): F[List[A]] =
      map(xs.foldLeft(pure(List.empty[A])) { (acc: F[List[A]], fa: F[A]) =>
        map2(acc, fa)((as, a) => a :: as)
      })(_.reverse)

    def traverse[A, B](xs: List[A])(f: A => F[B]): F[List[B]] = sequence(xs.map(f))

    def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)] = map2(fa, fb)(_ -> _)

  }

  object Applicative {

    def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

    implicit class ApplicativeOps[F[_], A](val fa: F[A]) extends AnyVal {
      def ap[B](fab: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fab)(fa)
      def fmap[B](f: A => B)(implicit F: Applicative[F]): F[B] = F.map(fa)(f)
      def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map2(fa, fb)(f)
    }

    implicit class Tupple2Syntax[F[_], A, B](val t: (F[A], F[B])) extends AnyVal {
      def mapN[C](f: (A, B) => C)(implicit F: Applicative[F]): F[C] = t match {
        case (fa, fb) => F.map2(fa, fb)(f)
      }
    }

    implicit class Tupple3Syntax[F[_], A, B, C](val t: (F[A], F[B], F[C])) extends AnyVal {
      def mapN[D](f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] = t match {
        case (fa, fb, fc) => F.map3(fa, fb, fc)(f)
      }

    }

    implicit val futureInstance: Applicative[Future] = new Applicative[Future] {
      def ap[A, B](fab: Future[A => B])(fa: Future[A]): Future[B] =
        for {
          f <- fab
          a <- fa
        } yield f(a)

      def pure[A](a: A): Future[A] = Future.successful(a)
    }

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

    implicit val applicativeEitherInstance: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
      def ap[A, B](fab: Either[String, A => B])(fa: Either[String, A]): Either[String, B] =
        for {
          f <- fab
          a <- fa
        } yield f(a)

      def pure[A](a: A): Either[String, A] = Right(a)
    }

    implicit class OptionOps[A](val a: A) extends AnyVal {
      def some: Option[A] = Some(a)
    }

  }

  import Applicative._

  case class Computation(a: Int, b: String, c: Float)

  import concurrent.ExecutionContext.Implicits._

  val computationF: Future[Computation] =
    Applicative[Future].map3(Future(1), Future("1"), Future(1.1f))(Computation.apply)

  val computation: Computation = Await.result(computationF, Duration.Inf)

  println(computation)

  import Applicative._

  val computation2F = (Future(1), Future("1"), Future(1.1f)).mapN(Computation.apply)

  println(Await.result(computation2F, Duration.Inf))

  println(Await.result(Future(1).map2(Future(2))(_ + _), Duration.Inf))

  val computation3F: Option[Computation] = Applicative[Option].map3(Some(1), Some("1"), None)(Computation.apply)

  def none[A]: Option[A] = None

  val computation4F = (Some(1): Option[Int], Some(2): Option[Int]).mapN(_ + _)
  val computation5F = (Option(1), Option(2)).mapN(_ + _)
  val computation6F = (None: Option[Int], Option(1)).mapN(_ + _)
  val computation7F = (none[Int], Option(1)).mapN(_ + _)
  val computation8F = (none[Int], 1.some).mapN(_ + _)

  println(computation5F)
  println("computation7F: " + computation7F)

  (Option(1), Option(null)) match {
    case (Some(a), Some(b)) => a + b
    case _                  => None
  }

  trait SemiGroup[A] {
    def combine(a: A, b: A): A
  }

  object Semigroup {
    def apply[A: SemiGroup]: SemiGroup[A] = implicitly[SemiGroup[A]]
  }

  // adt
  // * -> * -> *
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

  implicit def listSemigroupInstance[A]: SemiGroup[List[A]] = new SemiGroup[List[A]] {
    override def combine(a: List[A], b: List[A]): List[A] = a ++ b
  }

  object AdultDomain {

    case class Adult(age: Int, name: String)

    // * -> *
    type ValidationResult[A] = Validated[List[DomainValidation], A]

    implicit val applicativeInstance: Applicative[ValidationResult] = new Applicative[ValidationResult] {

      def pure[A](a: A): ValidationResult[A] = Valid(a)

      def ap[A, B](fab: ValidationResult[A => B])(fa: ValidationResult[A]): ValidationResult[B] = fa.ap(fab)
    }

    def validateAge(age: String): ValidationResult[Int] =
      Try(age.toInt) match {
        case Failure(_)                    => InValid(List(AgeIsNotANumber))
        case Success(value) if value >= 18 => Valid(value)
        case _                             => InValid(List(AgeIsTooLow))
      }

    def validateName(name: String): ValidationResult[String] =
      if (name.length <= 5) Valid(name) else InValid(List(NameTooLong))

    def adultForm(age: String, name: String): ValidationResult[Adult] =
      (validateAge(age), validateName(name)).mapN(Adult.apply)

    // validation error adt
    sealed trait DomainValidation

    case object AgeIsNotANumber extends DomainValidation
    case object AgeIsTooLow extends DomainValidation
    case object NameTooLong extends DomainValidation

  }

  import AdultDomain._

  val res: ValidationResult[Adult] = adultForm("11", "name")

  res match {
    case Valid(value) =>
      println("valid adult: " + value)
    case InValid(e) =>
      println(e)
      val xs: List[AdultDomain.AgeIsTooLow.type] = e.collect { case err: AgeIsTooLow.type => err }
      if (xs.nonEmpty) {
        println("You are not adult because age is too low")
      }
      val ys: List[AdultDomain.NameTooLong.type] = e.collect { case err: NameTooLong.type => err }
      if (ys.nonEmpty) {
        println("Please pass correct name. Must be shorter than 5")
      }
  }

  val ys: Future[List[Int]] = Applicative[Future].sequence(List(Future(1), Future(2), Future(3)))

  println(Await.result(ys, Duration.Inf))

  val ws = List("1", "2", "3a")

  def parse(s: String): Either[String, Int] = Try(s.toInt).toEither.left.map(_.getMessage)

  val us: Either[String, List[Int]] = Applicative[Either[String, *]].traverse(ws)(parse)

  println(us)

}
