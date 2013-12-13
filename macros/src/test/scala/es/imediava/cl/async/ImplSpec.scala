package es.imediava.cl.async

import org.specs2.mutable.Specification

class ImplSpec extends Specification {

  /**
   * val b = 2
   * val c = 3
   * val a = f(b + 1, c + 2)
   *
   * is converted to:
   *
   * val b = 2
   * val c = 3
   * val a$param1 = b + 1
   * val a$param2 = c + 2
   * val a = f(a$param1, a$param2)
   **/

  "Impl" should {

    "flatten expression in function calls" in {

      import scala.reflect.runtime.{universe => u}


      val initial = u reify {
        {
          def sum(a: Int, b: Int) = a + b
          val b = 2
          val c = 3
          val a = sum(b + 1, c + 2)
        }
      }

      println("initial")

      println(u showRaw initial.tree)
      println("")

      val expected = u reify {
        {
          def sum(a: Int, b: Int) = a + b
          val b = 2
          val c = 3
          val aParam1 = b + 1
          val aParam2 = c + 2
          val a = sum(aParam1, aParam2)
        }
      }

      println(u showRaw expected.tree)
      println("")

      Macros.hello(
        {
          def sum(a: Int, b: Int) = a + b
          val b = 2
          val c = 3
          val a = sum(b + 1, c + 2)
        }
      )

      ok
    }
  }


}
