package org.cric.main
import org.cric.`macro`.Macros._
object Main extends App {
  class A(val a : Int)
  class B(val b : Int) extends A(b)
  class C(val c: Int) extends B(c)
  val c = new C(10)
  println(accessors[C](c))
  println(accessors[B](c))
  println(accessors[A](c))

  // Your example:
  case class Person(name: String,
                    override val age: Int,
                    override val address: String
                   ) extends Details(age, address)

  class Details(val age: Int, val address: String)

  val person = Person("Alex", 33, "Europe")
  println(accessors[Details](person))
  println(accessors[Person](person))
}
