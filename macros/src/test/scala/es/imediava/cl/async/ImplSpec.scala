package es.imediava.cl.async

import org.specs2.mutable.Specification

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}
import scala.concurrent.{Future, Await}

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



    "deal with future that fails " in {
      val myException = new RuntimeException("future that failed")

      import Macros._
      import scala.concurrent.ExecutionContext.Implicits.global

      val result : Future[Boolean] = async {
        val f1 : Future[Boolean] = Future.failed[Boolean]{ new RuntimeException("future that failed") }
        var a1 = Macros.await(f1)
        a1
      }
      //scala.concurrent.Await.ready[Boolean](scala.concurrent.Future.apply[Boolean](true)(scala.concurrent.ExecutionContext.global), scala.concurrent.duration.DurationInt(5).seconds)
      result.isCompleted must_==(true)
      ok
    }



  }

  //Apply(
  // TypeApply(Select(Ident(es.imediava.cl.async.Macros), TermName("await")), List(TypeTree())), List(Apply(Apply(TypeApply(Select(Select(Select(Ident(scala), scala.concurrent), scala.concurrent.Future), TermName("apply")), List(TypeTree())),
  // List(Apply(Ident(TermName("sum")), List(Apply(Select(Ident(TermName("b")), TermName("$plus")), List(Literal(Constant(1)))), Apply(Select(Ident(TermName("c")), TermName("$plus")), List(Literal(Constant(2)))))))), List(Select(Select(Select(Select(Ident(scala), scala.concurrent), scala.concurrent.ExecutionContext), scala.concurrent.ExecutionContext.Implicits), TermName("global"))))))), Literal(Constant(()))))


}
