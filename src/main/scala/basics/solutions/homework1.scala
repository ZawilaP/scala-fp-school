package basics.solutions

import scala.annotation.tailrec

/**
  * Read:
  *   - https://profunctors.gitlab.io/introduction-to-functional-programming/d7bcb2c6da912e58fb28d4b2447b7b936f815102/fp/2020/07/22/referential-transparency.html
  *   - https://profunctors.gitlab.io/introduction-to-functional-programming/d7bcb2c6da912e58fb28d4b2447b7b936f815102/fp/2020/07/13/object-oriented-programming-vs-functional-programming.html
  *   - https://profunctors.gitlab.io/functional-programming-in-scala/d7bcb2c6da912e58fb28d4b2447b7b936f815102/2020/09/11/trampoline.html
  *
  * Implement
  *   - revers
  */
object homework1 extends App {

  //foldLeft
  def revers(s: String): String = {
    @tailrec
    def go(xs: List[Char], acc: String): String = xs match {
      case h :: t => go(t, h.toString ++ acc)
      case Nil    => acc
    }
    go(s.toList, "")
  }

  // "" => ""
  // "s"  => "s"
  // "helloWorld" => revers2("elloWorld") +  "h"
  def revers2(s: String): String =
    if (s.length <= 1) s
    else revers2(s.tail) + s.head

  println(revers("helloWorld"))
  println(revers2("helloWorld"))
  println(revers("helloWorld") == "dlroWolleh")

}
