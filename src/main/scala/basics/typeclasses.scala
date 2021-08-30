package basics

/**
  *  - what is a type class ?
  *  - type class building blocks by example (eq type class)
  *  - interface syntax
  *  - adhoc polymorphism
  *  - Show
  *  - Eq
  *  - Read
  */
object typeclasses extends App {

  trait Read[A] {
    def read(s : String) : A
  }

  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    def apply[A: Show]: Show[A] = implicitly[Show[A]]

    def apply2[A](implicit ev: Show[A]): Show[A] = ev

    object implicits {
      implicit class ShowOps[A](val a: A) extends AnyVal {
        def show(implicit S: Show[A]): String = S.show(a)
      }
    }

  }

  case class User(id: Long, name: String, password: String)

  object User {
    implicit val showInstance: Show[User] = new Show[User] {
      def show(a: User): String = {
        import a._
        s"""
           |id : $id
           |name : $name
           |password : ***
           |""".stripMargin
      }
    }
  }

  val user = User(1, "ssledz", "tajnehaslo")

  import Show.implicits._

  println(Show[User].show(user))
  println(Show.apply2[User].show(user))

  println(user.show)

  object admin {
    implicit val showInstance: Show[User] = new Show[User] {
      def show(a: User): String = {
        import a._
        s"""
           |id : $id
           |name : $name
           |password : $password
           |""".stripMargin
      }
    }
  }

  import admin.showInstance
  println(user.show)

  println(user.show(admin.showInstance))

  // bad, error !!!
  println(1 == "one")

  trait Eq[A] {
    def eq(a: A, b: A): Boolean
  }

  object Eq {
    def apply[A](implicit ev: Eq[A]): Eq[A] = ev

    object instances {
      implicit def fromEquals[A] : Eq[A] = new Eq[A] {
        override def eq(a: A, b: A): Boolean = a == b
      }
    }

    object syntax {
      implicit class EqOps[A](val a : A) extends AnyVal {
        def ===(b : A)(implicit ev : Eq[A]) : Boolean = ev.eq(a, b)
      }
    }
  }

  import Eq.instances._
  import Eq.syntax._

//  print(1 === "one")
  println(1 === 2)

  println(1.2d === 1)

  sealed trait Json {
    def noindent: String
  }

  object Json {
    def parse(s : String) : Json = ???
  }

  trait Encoder[A] {
    def toJson(a: A) : Json
  }

  trait Decoder[A] {
    def decode(a: Json) : A
  }

}
