package es.imediava.cl.async.utils

import scala.util.Success
import scala.reflect.macros.BlackboxMacro
import scala.concurrent.{Future, Promise}

/**
 * Created by zenexity on 24/12/13.
 */
trait BuildAutomaton extends BlackboxMacro {

  import c.universe._

  val nextState = {
    var count = 0
    (prefix: String) => {
      count = count + 1
      count
    }
  }

  val resumeName = TermName("resume$$async")

  def addVariable(clazz: ClassDef, varDef: ValDef) = {
    val q"class $name extends $extend { ..$body }" = clazz
    q"class $name extends $extend { ..${ varDef :: body } }"
  }

  /*def stateMachineSkeleton() = {
    val tree = reify {
      class MyStateMachine extends AnyRef {
        var f1 = Future.failed{ new RuntimeException("future that failed") }
        var result$async : scala.concurrent.Promise[Boolean] = scala.concurrent.Promise.apply[Boolean]();
        def resume$async : Unit = try {
          f1.onComplete(apply _)
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

  } */

  case class CaseResume(state: Int, body: Tree)

  def isAwait(func: Tree) = {
    func match {
      case q"await" => true
      case _ => false
    }
  }

  def createState(stat: Tree) = {
    //stat match {
     //case q"var $name : $tpe = $func(..$args)" if isAwait(func) =>
     //  val nextState = nextState()

     // Expected:
     //add to resumeAsync
     //  case 0:
     //    buildFuture().onComplete(this.apply)
     // add to apply(res: Try[])
     //  case 0:
     //    if (isFailure) {
     //      result$async.complete(tr.asInstanceOf[scala.util.Try[expr.tpe]])
     //      ()
     //    } else {
     //      await$1 = tr.get.asInstanceOf[stat.tpe]
     //      state = 1
     //      resume$async()
     //    }
     //    ()

    //}
  }

}
