package com.viadeo.data.main


import language.experimental.macros
import scala.reflect.macros.Context

import com.viadeo.data.model.NetworkPhotoNews
import com.viadeo.data.bean.NewPhotosNews
import scala.concurrent.Future

object Macros {
  def hello(param1: Unit): Unit = macro hello_impl

  def hello_impl(c: Context)(param1: c.Expr[Unit]): c.Expr[Unit] = {
    import c.universe._

    println(showRaw(param1.tree))
    param1.tree match {
      case Block(x :: xs, Literal(Constant(()))) =>
        x match {
          case q"def $name[..$tparams](...$vparamss): $tpt = $body" =>
            println(name)
          case q"$f(..$bar)" =>
            bar match {
              case v1 :: vs =>
            }
        }
        println("Yes it was that!!")
        reify { Unit }
      case _ =>
        println("It was other thing!!")
        reify { Unit }
    }
  }

  /*
   * val x = 1
   *
   * is converted to:
   *
   * val x : Int = 1
   */


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



  /**
   * val f1 = async { true }
   *
   * is converted to:
   *
   * val f1: scala.concurrent.Future[Boolean] = {
   *   scala.concurrent.Future.apply[Boolean](true)(scala.concurrent.ExecutionContext.Implicits.global)
   * };
   */
}