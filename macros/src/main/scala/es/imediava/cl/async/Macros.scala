package es.imediava.cl.async

import language.experimental.macros

import scala.reflect.macros.BlackboxContext
import scala.reflect.macros.BlackboxMacro
import scala.concurrent.Future

trait Impl extends BlackboxMacro {

  //Apply(
  //  Apply(
  //    Select(Select(Select(Select(Select(Select(Select(Ident($line71.$read), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("plus")),
  //      List(Apply(Select(Ident(TermName("a")), TermName("$plus")), List(Literal(Constant(2)))))),
  //  List(Apply(Select(Ident(TermName("b")), TermName("$plus")), List(Literal(Constant(2))))))

  def listToBlock(stats: List[c.Tree]) : c.Tree = {
    import c.universe._
    q"{..$stats}"
  }

  def flatten(b : c.Tree) : List[c.Tree] = {
    import c.universe._

    println(showRaw(b))
    b match {
      case q"$f($subArg)" =>
        val paramName = TermName("param1$1")
        val flattenedSubArg = q"{..${flatten(subArg)}}"
        flattenedSubArg match {
          case Block(stats, expr) => stats ++ (q"val $paramName: ${flattenedSubArg.tpe} = ${expr}" :: List(q"$f($paramName)"))
          case otherwise => q"val $paramName: ${flattenedSubArg.tpe} = ${otherwise}" :: List(q"$f($paramName)")
        }

      case otherwise : Tree => List(otherwise)
    }

  }

  def hello(param1: c.Expr[Unit]): c.Expr[Unit] = {
    import c.universe._

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
     * @param globalTree
     * @param app
     * @return
     */
    def flatten(app: Tree) = {

      val myNameGenerator = {
        var count = 0
        ()  => {
          count = count + 1
          s"param\$$count"
        }
      }


      def flatten(b : Tree) = {
        b match {
          case q"$f($subArg)" =>
            val paramName = TermName("param1$1")
            "val $paramName: ${subArg.tpe} = $subArg" :: List(q"$f($paramName)")
          case otherwise => otherwise
        }

      }
      /*def flattenArg(arg: Tree) : Block = {
        arg match {
          case q"$f($subArg)" =>
            val flattenSubArg = flattenArg(arg)
            q"val ${TermName(myNameGenerator())}: ${arg.tpe} = ${}"
          case otherwise : Tree => q"val ${TermName(myNameGenerator())}: ${otherwise.tpe} = $otherwise"
        }
      } */

      /*app match {
        case Apply(fun, args) =>
          val newTree = flatten(fun)
          args.map {
            case (tree, complexArg @ q"$f(..$args2)") =>
              val name = TermName(arg.toString + "1")
              val newVal = q"val $name: ${fun.tpe} = ${flatten(complexArg)}"
              //tree +:
          }
        case (tree, otherwise) => tree

      }*/
    }

    param1.tree match {
      case Block(stats, last) =>

        //stats.foreach(x => println(showRaw(x)))
        //println(showRaw(last))
        last match {
          case q"$func[..$tpe](..$bar2)" =>
            //bar2.foreach(x => println(showRaw(x)))
            //bar2.map{
              //flatten(param1.tree, _)
            //}

        }
        stats.map {
            case q"$func[..$tpe](..$bar2)" =>
              bar2.map{
                case q"$f(..$bar3)" =>
                    //println(s"Bar3:$bar3")
              }
            /*case q"$mods val $name: $tpe = $rhs" =>
              println(s"Rhs=$rhs")
              rhs match {
                case q"await(..$bar2)" =>
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
                  }*/



            case _ => Unit
        }

        reify { Unit }
      case _ =>
        reify { Unit }
    }
  }
}

object Macros {

  def hello(param1: Unit): Unit = macro Impl.hello

  def await[T](f: Future[T]) = ???

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