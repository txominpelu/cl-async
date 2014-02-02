/* Copyright (c) 2013 by Inigo Mediavilla
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
*/
package es.imediava.cl.async

import org.specs2.mutable.Specification

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}
import scala.concurrent.{Future, Await}

package es.imediava.cl.async.utils {
  object MyUtils {
    def myprint(s: String) = println(s)
  }
}

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




    "deal with one future " in {

      import Macros._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration.DurationInt
      import es.imediava.cl.async.utils.MyUtils._

      val result : Future[Boolean] = async {
        var f1 : Future[Boolean] = Future.apply(true)
        myprint("MyTal")
        var a1 = Macros.await(f1)
        a1
      }
      Await.result(result, DurationInt(2).seconds) must_==(true)
      ok
    }

    "deal with multiple futures" in {

      import Macros._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration.DurationInt

      val result : Future[Boolean] = async {
        var f1 : Future[Boolean] = Future.apply(true)
        var f2 : Future[Boolean] = Future.apply(false)
        var a1 = Macros.await(f1)
        var a2 = Macros.await(f2)
        a1 || a2
      }
      Await.result(result, DurationInt(2).seconds) must_==(true)
      ok
    }



  }

  //Apply(
  // TypeApply(Select(Ident(es.imediava.cl.async.Macros), TermName("await")), List(TypeTree())), List(Apply(Apply(TypeApply(Select(Select(Select(Ident(scala), scala.concurrent), scala.concurrent.Future), TermName("apply")), List(TypeTree())),
  // List(Apply(Ident(TermName("sum")), List(Apply(Select(Ident(TermName("b")), TermName("$plus")), List(Literal(Constant(1)))), Apply(Select(Ident(TermName("c")), TermName("$plus")), List(Literal(Constant(2)))))))), List(Select(Select(Select(Select(Ident(scala), scala.concurrent), scala.concurrent.ExecutionContext), scala.concurrent.ExecutionContext.Implicits), TermName("global"))))))), Literal(Constant(()))))


}
