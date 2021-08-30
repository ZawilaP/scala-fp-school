package basics.solutions

/**
  * Read:
  *   - https://stackoverflow.com/questions/17270003/why-are-classes-inside-scala-package-objects-dispreferred
  *   - https://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html
  *   - https://github.com/typelevel/simulacrum
  *   - https://github.com/ssledz/fp-toy
  *
  *  Watch:
  *   - https://www.youtube.com/watch?v=V10hzjgoklA&list=PLw1ByaFQI-9ICX6a6T4f2OW-w9SV17MUg&index=1&t=1609s&ab_channel=LambdaWorld
  *   - https://www.youtube.com/watch?v=yjmKMhJOJos&ab_channel=YOW%21Conferences
  *
  *  Implement
  *   - create Ord type class
  *      - add syntax support
  *      - implement instances for Int and String
  *      - try to choose one primitive operation and implement all in terms of this primitive op
  *      - implements sort (using any algorithm)
  *    - create Read type class
  *      - add syntax support
  *      - implement instances for Int and Foo case class (read should be opposite to the toString method)
  *     - Bonus
  *       - create Read and Ord using simulacrum
  *       - create simple code formatter, more on https://gist.github.com/ssledz/d332ab20d78e5502d8ba7d92ff934c46
  */
object homework3 extends App {

  trait Ord[A] {
    def ge(a: A, b: A): Boolean = !le(a, b) // >=
    def le(a: A, b: A): Boolean = !ge(a, b) // <=
    def lt(a: A, b: A): Boolean = le(a, b) && !(ge(a, b) == le(a, b)) // <
    def gt(a: A, b: A): Boolean = !lt(a, b) // >
  }

  object Ord {
    def apply[A: Ord]: Ord[A] = implicitly[Ord[A]]

    implicit class OrdOps[A](val a: A) extends AnyVal {
      def >=(b: A)(implicit ev: Ord[A]): Boolean = ev.ge(a, b)
      def <=(b: A)(implicit ev: Ord[A]): Boolean = ev.le(a, b)
      def <(b: A)(implicit ev: Ord[A]): Boolean = ev.lt(a, b)
      def >(b: A)(implicit ev: Ord[A]): Boolean = ev.gt(a, b)
//      def ===(b: A)(implicit ev: Ord[A]): Boolean = ev.le(a, b) && ev.ge(a, b) // this is not a part of Ord
    }
  }

  implicit val intOrd: Ord[Int] = new Ord[Int] {
    override def ge(a: Int, b: Int): Boolean = a >= b
  }

  def sort[A: Ord](xs: List[A]): List[A] = {
    import Ord._
    def qsort(xs: List[A]): List[A] = xs match {
      case Nil => List.empty
      case h :: t =>
        val left = qsort(t.filter(_ <= h))
        val right = qsort(t.filter(_ > h))
        left ::: List(h) ::: right
    }
    qsort(xs)
  }

  assert(sort(List(5, 4, 1, 3, 2, 0)) == List(0, 1, 2, 3, 4, 5))

  trait Read[A] {
    def read(s: String): A
  }

  object Read {
    def apply[A: Read]: Read[A] = implicitly[Read[A]]

    implicit class ReadOps(val s: String) extends AnyVal {
      def read[A: Read]: A = Read[A].read(s)
    }
  }

  implicit val intRead: Read[Int] = (s: String) => s.toInt

  val someInt = 12456

  assert(implicitly[Read[Int]].read(someInt.toString) == someInt)

  import Read._

  assert("123456789".read[Int] == 123456789)

  case class Foo(a: Int, b: String, c: List[String])

  implicit def fooRead(implicit ev: Read[List[String]]): Read[Foo] = new Read[Foo] {
    def read(s: String): Foo = {
      val a :: b :: cs = s
        .stripPrefix("Foo")
        .stripPrefix("(")
        .stripSuffix(")")
        .split(",")
        .map(_.trim)
        .toList
      Foo(a.read[Int], b, cs.mkString(",").read[List[String]])
    }
  }

  object ListInstance {
    implicit val strListRead: Read[List[String]] = (s: String) =>
      s.stripPrefix("List")
        .stripPrefix("(")
        .stripSuffix(")")
        .split(",")
        .map(_.trim)
        .toList
  }

  import ListInstance._

  val foo = Foo(1, "one", List("one", "two", "three"))

  assert(implicitly[Read[Foo]].read(foo.toString) == foo)

  assert(foo.toString.read[Foo] == foo)

}
