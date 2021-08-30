package basics

/**
  *  - function definition
  *  - closure
  *  - function application
  *  - function composition
  *  - pure function
  *  - side effects
  *  - referentially transparency
  *  - function as a value
  *  - point free style
  *  - recursion (tail call, trampoline)
  *  - function literals
  *  - higher order functions
  *  - parameters list
  *  - variadic functions
  *  - currying
  *  - partial application
  *  - partial function -> total Function
  *  - parametric, adhoc and subtyping polymorphism (https://medium.com/@sinisalouc/ad-hoc-polymorphism-and-type-classes-442ae22e5342)
  *  - strict vs non-strict functions
  */
object functions extends App {

  // function definition
  def foo(x: Int): Int = x

  // closure
  val y = 1
  def bar(x: Int): Int = x + y

  // function application
  println(bar(1))

  // f(x) = x ^ 2 + 2
  def f1(x: Int): Int = x * x + 2

  def square(x: Int): Int = x * x
  def add2(x: Int): Int = x + 2

  // function composition
  def f2(x: Int): Int = add2(square(x))
  def f3(x: Int): Int = (square _).andThen(add2 _)(x)
  def f4 = (square _).andThen(add2 _) // point free style
  val f5 = (square _).andThen(add2 _) // point free style
  println(f5(1) == f1(1))

  // pure function
  // memoization
  def g(x: Int): Int = x + 1

  var z = 0
  def gg(x: Int): Int = x + z // not pure function (but with no side effects)

  println(gg(1))
  z = 1
  println(gg(1))

  // side effects
  def h(x: Int): Int = {
    println(x)
    x + 1
  }

  h(1)
  h(1)

  // referentially transparency
  def k(x: Int): Int = x + 1
  val kk = k(1)
  val y1 = kk // val y1 = k(1)
  val y2 = kk + 1 // val y2 = k(1) + 1
  val zz = y1 + y2

  val zzz = 1
  def k2(x: Int): Int = x + 1 + zzz // referentially transparent but not purre

  // function as a value
  val bar2: Int => Int = x => x + 1

  // recursion
  // n == 0 lub 1 => n! = 1             base
  // n > 1        => n! = n * (n-1)!    general
  def fact(n: Int): Int = if (n <= 1) 1 else n * fact(n - 1)

  println(fact(2))

  // function literals
  val buzz: Int => Int = x => x + 1

  // higher order functions
  def mapSum(xs: List[Int], f: Int => Int): Int = xs.map(f).sum

  println(mapSum(List(1, 2, 3), _ + 1))

  // parameters list
  def mapSum2(xs: List[Int])(f: Int => Int): Int = xs.map(f).sum

  mapSum2(List(1, 2, 3)) { x =>
    x + 1 + Math.random().toInt
  }

  // variadic functions
  def buzz4(xs: Int*): Seq[Int] = xs

  println(buzz4(1, 2, 3))
  println(buzz4(1))
  println(buzz4())

  // currying
  // f : (A, B) => C      g = f.curry == A => (B => C) === A => B => C

  def add(a: Int, b: Int): Int = a + b
  val addCurried: Int => Int => Int = (add _).curried

  // partial application
  val inc: Int => Int = addCurried(1)

  println(inc(1))
  println(inc(2))

  val inc2 = add(1, _)
  val inc3 = (add _).curried(1)
  val inc4 = add(_, 1)

  // partial function -> total Function
  def div(a: Float, b: Float): Option[Float] = if (b != 0) Some(a / b) else None

  // partial function
  val div2: PartialFunction[(Float, Float), Float] = {
    case (a, b) if b != 0 => a / b
  }

  println(div2((1, 2)))
  println(div2.isDefinedAt((1, 0)))

  def x7(a : Float, b : Float) : Option[Float] = if(div2.isDefinedAt(a, b)) Some(div2((a, b))) else None

  println(x7(1, 0))

  val div3: ((Float, Float)) => Option[Float] = div2.lift

  println(div3((1, 0)))

  // parametric, adhoc and subtyping polymorphism

  def headInt(xs: List[Int]) : Int = xs.head
  def headFloat(xs: List[Float]) : Float = xs.head
  def headDouble(xs: List[Double]) : Double = xs.head

  // parametric polymorphism
  def head[A](xs: List[A]) : A = xs.head

  // subtyping polymorphism -> object oriented programming

  // adhoc polymorphism

  // type class
  trait ToJson[A] {
    def toJson(a: A) : String
  }

  lazy  implicit val IntJson: ToJson[Int] = ???

  def toJson[A : ToJson](a : A) : String = implicitly[ToJson[A]].toJson(a)

  toJson(1)

  // strict vs non-strict functions

  def fooBar(a: => Boolean, b : Boolean) : Boolean = if(b) a else false

  fooBar(1 == 2 || 2 == 2, 1 == 4)

}
