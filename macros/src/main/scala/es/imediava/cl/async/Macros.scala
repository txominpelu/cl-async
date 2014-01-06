package es.imediava.cl.async

import language.experimental.macros

import scala.reflect.macros.BlackboxMacro
import scala.reflect.macros.BlackboxContext
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, DurationInt}
import es.imediava.cl.async.utils.BuildAutomaton
import scala.util.Success

import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers
import scala.annotation.compileTimeOnly


trait FlattenFunctionCalls extends BlackboxMacro {

  import c.universe._

  def listToBlock(stats: List[c.Tree]) : c.Tree = {
    import c.universe._
    q"{..$stats}"
  }

  def blockToList(tree: c.Tree): List[c.Tree] = tree match {
    case Block(stats, expr) => stats :+ expr
    case t                  => t :: Nil
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

trait AsyncMacro extends BlackboxMacro with TypingTransformers {

  import c.universe._
  import definitions._

  import language.reflectiveCalls
  val powerContext = c.asInstanceOf[c.type { val universe: Global; val callsiteTyper: universe.analyzer.Typer }]

  val global: powerContext.universe.type   = powerContext.universe
  val callSiteTyper: global.analyzer.Typer = powerContext.callsiteTyper
  val macroApplication: global.Tree        = c.macroApplication.asInstanceOf[global.Tree]

  lazy val macroPos = macroApplication.pos.makeTransparent
  def atMacroPos(t: global.Tree) = global.atPos(macroPos)(t)

  def listToBlock(stats: List[c.Tree]) : c.Tree = {
    import c.universe._
    q"{..$stats}"
  }

  def blockToList(tree: c.Tree): List[c.Tree] = tree match {
    case Block(stats, expr) => stats :+ expr
    case t                  => t :: Nil
  }

  def asyncImpl(value: c.Expr[Boolean]) : c.Expr[Future[Boolean]] = {
    //ValDef(Modifiers(), TermName("a2"), TypeTree().setOriginal(Select(Ident(scala), scala.Boolean)), Apply(Select(Ident(es.imediava.cl.async.Macros), TermName("await")), List(Ident(TermName("f1"))))))
    stateMachineSkeleton()
  }

  def stateMachineSkeleton() = {
    val tree = reify {
      class MyStateMachine extends AnyRef {
        var f1 = Future.failed{ new java.lang.RuntimeException("future that failed") }
        var result$async : scala.concurrent.Promise[Boolean] = scala.concurrent.Promise.apply[Boolean]();
        def resume$async : Unit = try {
          f1.onComplete(apply _)(scala.concurrent.ExecutionContext.global)
        } catch {
          case scala.util.control.NonFatal((tr @ _)) => {
            {
              result$async.complete(scala.util.Failure.apply(tr));
              ()
            };
            ()
          }
        };

        def apply(result : scala.util.Try[Boolean]): Unit = {
          result$async.complete(result)
        }
      }
      val myVar = new MyStateMachine()
      myVar.resume$async
      myVar.result$async.future
    }
    tree

  }

  def applyOnComplete() : Expr[Unit] = {
    val result = q"result$$async"
    val tr = q"tr"
    reify {
        val state = 0
        state match {
          case 0 =>
            if (c.Expr[scala.util.Try[Boolean]](tr).splice.isFailure)
            {
              c.Expr[scala.concurrent.Promise[Boolean]](result).splice.complete(tr.asInstanceOf[scala.util.Try[Boolean]]);
              ()
            }
            else
            {
              c.Expr[scala.concurrent.Promise[Boolean]](result).splice.complete(tr.asInstanceOf[scala.util.Try[Boolean]]);
            };
            ()
        }

    }
  }


}

object Macros {

  @compileTimeOnly("`await` must be enclosed in an `async` block")
  def await(f: Future[Boolean]) : Boolean = ???

  def async(value: Boolean) : Future[Boolean] = macro AsyncMacro.asyncImpl

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