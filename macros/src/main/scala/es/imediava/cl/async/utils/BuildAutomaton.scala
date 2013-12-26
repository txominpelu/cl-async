package es.imediava.cl.async.utils

/**
 * Created by zenexity on 24/12/13.
 */
object BuildAutomaton {

  import scala.reflect.runtime.universe._

  val nextState = {
    var count = 0
    (prefix: String) => {
      count = count + 1
      count
    }
  }

  val resumeName = TermName("resume$$async")

  def stateMachineSkeleton() = {
    q"""class MyStateMachine extends AnyRef {
       var state$$async: Int = 0;
       var result$$async: scala.concurrent.Promise[Int] = scala.concurrent.Promise.apply[Int]();
       def $resumeName : Unit = try {
            state$$async match {
              case _ => ()
            }
          } catch {
            case scala.util.control.NonFatal((tr @ _)) => {
              {
                result$$async.complete(scala.util.Failure.apply(tr));
                ()
              };
              ()
            }
          };

       def apply(tr: scala.util.Try[Any]): Unit = state$$async match {
            case _ => ()
       }
       def apply: Unit = resume$$async()
    }
     """

  }

  def addVariable(clazz: ClassDef, varDef: ValDef) = {
    val q"class $name extends $extend { ..$body }" = clazz
    q"class $name extends $extend { ..${ varDef :: body } }"
  }

  case class CaseResume(state: Int, body: Tree)

  def addMethodToResume(clazz: ClassDef, defDef: DefDef) = {

  }

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
