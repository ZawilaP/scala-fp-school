package basics

/**
  * types of implicits
  *  - implicit parameters
  *  - implicit conversion
  *  - implicit conversions as implicit parameters
  *  - context bounds
  *
  * where do implicits come from? (https://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html)
  *  - implicits defined in lexical scope
  *  - implicits defined in implicit scope
  *    - companion object
  *    - package objects
  */
object implicits extends App {

  case class Foo(x: String) extends AnyVal

  def foo(a: String)(implicit b: Foo): String = a + b.x

  foo("Hello")(Foo("s"))

  implicit val fooVal = Foo(" implicit")

  println(foo("Hello"))

  // implicit conversion ex OptionOpts

  class IntWrapper(val x: Int) extends AnyVal

  def add(a: Int, b: String)(implicit f: String => IntWrapper): Int =
    a + b.x

  implicit def toInt2(a: String): IntWrapper = new IntWrapper(a.toInt)

  println(add(1, "2"))

  class Bar(x: Int, y: Int) {
    val z = x + y
    def foo: Int = x
  }

  val bar = new Bar(1, 2)
  bar.z

  def addInt(x: Int, y: Int): Int = x + y
  def addString(x: String, y: String): String = x + y

  trait SemiGroup[A] {
    def combine(a: A, b: A): A
  }

  object SemiGroup {
    def apply[A](implicit S: SemiGroup[A]): SemiGroup[A] = S
  }

  implicit val stringSemigroup: SemiGroup[String] = new SemiGroup[String] {
    def combine(a: String, b: String): String = a + b
  }

  implicit val intSemigroup: SemiGroup[Int] = new SemiGroup[Int] {
    def combine(a: Int, b: Int): Int = a + b
  }

  def add[A](x: A, y: A)(implicit F: SemiGroup[A]): A = F.combine(x, y)

  println(add("hello", "type classes"))

  println(add(1, 2))

  def add2[A: SemiGroup](x: A, y: A): A = implicitly[SemiGroup[A]].combine(x, y)

  println(add2(1, 2))

  class FooBar2[A : SemiGroup]
  class FooBar[A](implicit F : SemiGroup[A])

  def add3[A: SemiGroup](x: A, y: A): A = SemiGroup[A].combine(x, y)


  println(add3(1, 2))

  sealed trait BarUniversalTrait extends Any
  case class Foo2(s : String) extends AnyVal with BarUniversalTrait
  case class FooBar3(i : Int) extends AnyVal with BarUniversalTrait
  object Foo2 {
    implicit val semiGroupInstance: SemiGroup[Foo2] = new SemiGroup[Foo2] {
      def combine(a: Foo2, b: Foo2): Foo2 = Foo2(a.s ++ b.s)
    }
  }

  val foo2OrBar : BarUniversalTrait = FooBar3(1)


  SemiGroup[Foo2].combine(new Foo2(""), new Foo2("ss"))

}
