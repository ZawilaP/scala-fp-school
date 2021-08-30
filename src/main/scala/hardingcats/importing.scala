package hardingcats

/**
  *  - imports all of Cats’ type classes in one go
  *  - imports all of Cats’ data types in one go
  *  - importing all of the standard type class instances and all of the syntax in one go.
  *  - imports all of the syntax in one go
  *  - selective syntax import
  */
object importing extends App {

  // noobs
  {
    import cats._ // imports all of Cats’ type classes in one go
    import cats.implicits._ // importing all of the standard type class instances and all of the syntax in one go.

    case class Foo(a: Int, s: String)

    object Foo {
      implicit val showInstance: Show[Foo] = Show.fromToString
    }

    println(Foo(1, "message").show)

  }

  {
    import cats._ // imports all of Cats’ type classes in one go
    import cats.syntax.all._ // imports all of the syntax in one go
    import cats.instances.all._

    println(Show[Int].show(9))
    println(1234.show)

  }

  {
    import cats.syntax.show._ // selective syntax import

    println(12.4.show)

  }

  // pro
  {
    import cats._
    import cats.instances.bigDecimal._
    import cats.syntax.eq._
    import cats.syntax.show._


    println(BigDecimal(122).show)
    Eq[Int].eqv(11, 12)
    println(12 === 12)
  }

}
