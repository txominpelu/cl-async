package es.imediava.cl.async

import language.experimental.macros

import scala.reflect.macros.BlackboxContext
import scala.reflect.macros.BlackboxMacro

trait Impl extends BlackboxMacro {

  //Apply(Ident(TermName("sum")), List(Apply(Select(Ident(TermName("b")), TermName("$plus")), List(Literal(Constant(1)))),
    //Apply(Select(Ident(TermName("c")), TermName("$plus")), List(Literal(Constant(2)))))))), Literal(Constant(())))

  def hello(param1: c.Expr[Unit]): c.Expr[Unit] = {
    import c.universe._

    param1.tree match {
      case Block(stats, Literal(Constant(()))) =>
        stats.map { x =>
          println(showRaw(x))
          x match {
            case q"def $name[..$tparams](...$vparamss): $tpt = $body" =>
              println(name)
            case q"$mods val $name: $tpe = $rhs" =>
              println(s"Rhs=$rhs")
              rhs match {
                case q"$f(..$bar2)" =>
                  // I need to lift bar2 => bar2 = f(a + 2, b + 2)
                  // I need to add two vars :
                  // aParam1 = a + 2
                  // aParam2 = b + 2
                  // and replace the call f(a+2, b+2)
                  // by f(aParam1, aParam2)
                  bar2.map{ x =>
                    x match {
                      case q"$f(..$bar3)" =>
                        println(s"Bar3:$bar3")
                    }
                  }
                case List() => Unit
              }


            case List() => Unit
          }
        }

        reify { Unit }
      case _ =>
        reify { Unit }
    }
  }
}

object Macros {

  def hello(param1: Unit): Unit = macro Impl.hello

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