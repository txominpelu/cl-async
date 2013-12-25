package es.imediava.cl.async

import language.experimental.macros

import scala.reflect.macros.BlackboxMacro
import scala.concurrent.Future

trait FlattenFunctionCalls extends BlackboxMacro {

  import c.universe._

  def listToBlock(stats: List[c.Tree]) : c.Tree = {
    import c.universe._
    q"{..$stats}"
  }

  val nameGenerator = {
    var count = 0
    (prefix: String) => {
      count = count + 1
      s"$prefix$$$count"
    }
  }

  def flattenSubElem(subElem: c.Tree, paramName: TermName) = {
    val flattenedSubArg = q"{..${flatten(subElem)}}"
    flattenedSubArg match {
      case Block(stats, expr) => stats ++ (q"val $paramName: ${flattenedSubArg.tpe} = ${expr}" :: Nil)
      case otherwise => q"val $paramName: ${flattenedSubArg.tpe} = ${otherwise}" :: Nil
    }
  }

  /**
   * Takes the current tree that represents an application
   * and return a tree that has a flattened version of the application.
   *
   * e.g.
   * { sum(a + (1 + 2), b + 2) }
   *
   * is converted to
   *
   * { val param1$1 = 1 + 2
   *   val param1$2 = a + param1$1
   *   val param2$1 = b + 2
   *   sum(param1$2, param2$1) }
   *
   * @param b Entry tree
   * @return
   */
  def flatten(b : c.Tree) : List[c.Tree] = {
    //import c.universe._
    b match {
      case q"$f(..$subArgs)" if subArgs.length > 0 =>
        val functName = TermName(nameGenerator("function"))
        val flattenedArgs = subArgs.map{ subArg =>
          val paramName = TermName(nameGenerator("param"))
          (q"$paramName", flattenSubElem(subArg, paramName))
        }
        val paramNames = flattenedArgs.map(_._1)
        flattenedArgs.map(_._2).flatten ++ flattenSubElem(f, functName) ++ List(q"$functName(..$paramNames)")
      case otherwise : Tree => List(otherwise)
    }

  }

  def unit(param1: c.Expr[Unit]) = { reify { () } }


}

object Macros {

  def hello(param1: Unit): Unit = macro FlattenFunctionCalls.unit

  def await[T](f: Future[T]) : T = ???

  def async[T](value: T) : Future[T] = ???

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