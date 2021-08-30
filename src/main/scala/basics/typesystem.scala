package basics

/**
  *  - type hierarchy
  *    - bottom type ->  subtype of all types
  *       - example of usage
  *         * signal that function does not terminate
  *         * useful in typing the "leaf nodes" of polymorphic data structures
  *     - unit type
  *  - generic classes
  *  - type constructor
  *  - higher-kinded type
  *  - variances
  *     - covariant
  *     - contravariant
  *     - invariant
  */
object typesystem extends App {

  sealed trait List[+A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  val xs: List[Int] = Nil

  def infLoop: Nothing = {
    while (true) {}
    throw new RuntimeException("can't be")
  }

  val x: Unit = ()

  def sideEffect(): Unit = {
    // do nasty things
  }

  class Foo[A] // type constructor       f : type A => type Foo[A]   kind: * -> *
  def type1: Foo[Int] = ??? // concrete type Foo[Int]
  class Foo2[A, B, C] // type constructor      kind: * -> * -> * -> *
  // Int, String, Float - types    kind: () -> *

  // Kinded ~ type constructor

  def type2: Foo2[Int, String, Int] = ??? // concrete type

  object Sandbox {

    type Foo2Int[B, C] = Foo2[Int, B, C] // * -> * -> *
    type Foo2IntString[C] = Foo2[Int, String, C] // * -> *
    type Foo2IntStringInt = Foo2[Int, String, Int] // () -> *
    type Foo2StringInt[A] = Foo2[A, String, Int] // * -> *

    def foo[A: Foo]: String = ???
    def foo2[A: Foo2[*, String, Int]]: String = ???
    def foo3[A: Foo2StringInt]: String = ???

  }

  trait MyFunction[-A, +B] {
    def apply(a : A) : B
  }

  trait MyString
  trait Text extends MyString

  trait Number
  trait BigNumber extends Number


  def f : MyFunction[MyString, Number] = ???
  def g: MyFunction[MyString, BigNumber] = ???
  def h: MyFunction[Text, BigNumber] = ???

  def a : MyFunction[MyString, Number] = f
  def b : MyFunction[MyString, Number] = g
//  def c : MyFunction[MyString, Number] = h
  def c : MyFunction[Text, Number] = h
  def d : MyFunction[Text, Number] = g


}
