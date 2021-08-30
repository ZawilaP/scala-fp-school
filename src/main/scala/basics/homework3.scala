package basics

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
    def ge(a: A, b: A): Boolean // >=
    def le(a: A, b: A): Boolean // <=
    def lt(a: A, b: A): Boolean // <
    def gt(a: A, b: A): Boolean // >
  }

  implicit val intOrd: Ord[Int] = ???

  def sort[A: Ord](xs: List[A]): List[A] = ???

  assert(sort(List(5, 4, 1, 3, 2, 0)) == List(0, 1, 2, 3, 4, 5))

  case class Foo(a: Int, b: String, c: List[String])

  trait Read[A] {
    def read(s: String): A
  }

  implicit val fooRead: Read[Foo] = ???
  implicit val intRead: Read[Int] = ???

  val foo = Foo(1, "one", List("one", "two", "three"))
  val someInt = 12456

  assert(implicitly[Read[Foo]].read(foo.toString) == foo)
  assert(implicitly[Read[Int]].read(someInt.toString) == someInt)

}
