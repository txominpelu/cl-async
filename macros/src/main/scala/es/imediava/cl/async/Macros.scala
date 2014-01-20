package es.imediava.cl.async

import language.experimental.macros

import scala.reflect.macros.BlackboxMacro
import scala.reflect.macros.BlackboxContext
import scala.concurrent.{ExecutionContext, Promise, Await, Future}
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
object AsyncMacro {
  def apply(c: BlackboxContext): AsyncMacro = {
    import language.reflectiveCalls
    val powerContext = c.asInstanceOf[c.type { val universe: Global; val callsiteTyper: universe.analyzer.Typer }]
    new AsyncMacro {
      val global: powerContext.universe.type   = powerContext.universe
      val callSiteTyper: global.analyzer.Typer = powerContext.callsiteTyper
      val macroApplication: global.Tree        = c.macroApplication.asInstanceOf[global.Tree]
    }
  }
}

private[async] trait AsyncMacro
  extends TypingTransformers {

  val global: Global
  val callSiteTyper: global.analyzer.Typer
  val macroApplication: global.Tree

  lazy val macroPos = macroApplication.pos.makeTransparent
  def atMacroPos(t: global.Tree) = global.atPos(macroPos)(t)

  import global._

  abstract class MacroTypingTransformer extends TypingTransformer(callSiteTyper.context.unit) {
    currentOwner = callSiteTyper.context.owner
    curTree = EmptyTree

    def currOwner: Symbol = currentOwner

    localTyper = global.analyzer.newTyper(callSiteTyper.context.make(unit = callSiteTyper.context.unit))
  }

  def transformAt(tree: Tree)(f: PartialFunction[Tree, (analyzer.Context => Tree)]) = {
    object trans extends MacroTypingTransformer {
      override def transform(tree: Tree): Tree = {
        if (f.isDefinedAt(tree)) {
          f(tree)(localTyper.context)
        } else super.transform(tree)
      }
    }
    trans.transform(tree)
  }

}

trait AsyncMacroInigo extends BlackboxMacro {

  val asyncMacro = AsyncMacro(c)

  val universe: reflect.internal.SymbolTable = asyncMacro.global

  import asyncMacro.global._

  def Expr[T: WeakTypeTag](tree: Tree): Expr[T] = asyncMacro.global.Expr[T](rootMirror, asyncMacro.global.FixedMirrorTreeCreator(rootMirror, tree))

  case class SymLookup(stateMachineClass: Symbol, applyTrParam: Symbol) {
    def stateMachineMember(name: TermName): Symbol = {
      println(stateMachineClass)
      stateMachineClass.info.member(name)
    }

    def memberRef(name: TermName): Tree =
      gen.mkAttributedRef(stateMachineMember(name))
  }

  def listToBlock(stats: List[Tree]) : Tree = {
    q"{..$stats}"
  }

  def blockToList(tree: Tree): List[Tree] = tree match {
    case Block(stats, expr) => stats :+ expr
    case t                  => t :: Nil
  }

  def isAwait(tree: Tree) = {
    tree match {
      // FIXME: find a way to identify our await uniquely (async uses a global tree)
      case Apply(Select(Ident(TermName(t1)), TermName(name)), _) if name == "await" => true
      case _ => false
    }
  }


  def change(sym: Symbol, classSkeleton : Tree) = {
    if (sym != NoSymbol)
      sym.owner = classSkeleton.symbol
  }

  def asyncImpl(value: c.Expr[Boolean]) : c.Expr[Future[Boolean]] = {
    val skeleton = stateMachineSkeleton().tree
    val classSkeleton = asyncMacro.callSiteTyper.typedPos(skeleton.pos)(skeleton).setType(skeleton.tpe).asInstanceOf[Block].stats.head
    // Create `ClassDef` of state machine with empty method bodies for `resume` and `apply`.
    val sLookup = SymLookup(classSkeleton.symbol, applyDefDefDummyBody.vparamss.head.head.symbol)

    val awaitCall = blockToList(value.tree.asInstanceOf[Tree]).collectFirst{
      case valDef @ ValDef (_ , _, _, call @ Apply(method, param1 :: Nil)) if isAwait(call) => Awaitable(param1, sLookup.stateMachineMember(TermName("result")), valDef )
    }

    val awaitState = new AsyncStateWithAwait(Nil, 0, 1, awaitCall.get, sLookup)
    val resumeAsyncDef = buildDef(sLookup.stateMachineMember("resume$async"), awaitState.mkHandlerCaseForState :: Nil)

    println("Hello")
    change(resumeAsyncDef.symbol, classSkeleton)
    println(s"Prev Class=${showRaw(classSkeleton)}")
    val newClassSkeleton = asyncMacro.callSiteTyper.typedPos(classSkeleton.pos)(classSkeleton).setType(classSkeleton.tpe)
    val finalTree = asyncMacro.transformAt(newClassSkeleton){
      case dd@DefDef(_, TermName("resume$async"), _, List(_), _, _) if dd.symbol.owner == newClassSkeleton.symbol =>
        (ctx: analyzer.Context) =>
          treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, resumeAsyncDef.rhs)
    }
    println(showRaw(s"Final Class=${finalTree}"))
    awaitCall.get.asInstanceOf[c.Expr[Future[Boolean]]]
  }

  private def literalUnit = Literal(Constant(()))

  private def mkHandlerCase(num: Int, rhs: List[Tree]): CaseDef =
    mkHandlerCase(num, Block(rhs, literalUnit))

  private def mkHandlerCase(num: Int, rhs: Tree): CaseDef =
    CaseDef(Literal(Constant(num)), EmptyTree, rhs)

  case class Awaitable(expr: Tree, resultName: Symbol, resultValDef: ValDef)

  val applyDefDefDummyBody: DefDef = {
    val applyVParamss = List(List(ValDef(Modifiers(Flag.PARAM), TermName("tr"), TypeTree(tryType[Any]), EmptyTree)))
    DefDef(NoMods, TermName("apply"), Nil, applyVParamss, TypeTree(definitions.UnitTpe), Literal(Constant(())))
  }

  trait AsyncState {
    def state: Int

    def nextStates: List[Int]

    def mkHandlerCaseForState: CaseDef

    def mkOnCompleteHandler[T: WeakTypeTag]: Option[CaseDef] = None

    var stats: List[Tree]

    final def allStats: List[Tree] = this match {
      case a: AsyncStateWithAwait => stats :+ a.awaitable.resultValDef
      case _ => stats
    }

    final def body: Tree = stats match {
      case stat :: Nil => stat
      case init :+ last => Block(init, last)
    }
  }


  final class AsyncStateWithAwait(var stats: List[Tree], val state: Int, nextState: Int,
                                  val awaitable: Awaitable, symLookup: SymLookup)
    extends AsyncState {

    def nextStates: List[Int] =
      List(nextState)

    override def mkHandlerCaseForState: CaseDef = {
      val callOnComplete = onComplete(Expr(awaitable.expr),Expr(This(tpnme.EMPTY))).tree
      mkHandlerCase(state, stats :+ callOnComplete)
    }

    override def mkOnCompleteHandler[T: WeakTypeTag]: Option[CaseDef] = {
      val tryGetTree =
        Assign(
          Ident(awaitable.resultName),
          TypeApply(Select(tryyGet[T](Expr[scala.util.Try[T]](Ident(symLookup.applyTrParam))).tree, newTermName("asInstanceOf")), List(TypeTree(typeOf[Boolean])))
        )

      /* if (tr.isFailure)
       *   result.complete(tr.asInstanceOf[Try[T]])
       * else {
       *   <resultName> = tr.get.asInstanceOf[<resultType>]
       *   <nextState>
       *   <mkResumeApply>
       * }
       */
      val ifIsFailureTree =
        If(tryyIsFailure(Expr[scala.util.Try[T]](Ident(symLookup.applyTrParam))).tree,
          completeProm[T](
            Expr[Promise[T]](symLookup.memberRef(TermName("result"))),
            Expr[scala.util.Try[T]](
              TypeApply(Select(Ident(symLookup.applyTrParam), newTermName("asInstanceOf")),
                List(TypeTree(tryType[T]))))).tree,
          Block(List(tryGetTree, mkStateTree(nextState, symLookup)), mkResumeApply(symLookup))
        )

      Some(mkHandlerCase(state, List(ifIsFailureTree)))
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  def applyOnCompleteFunc (elseExpr: Expr[Unit], promise: Expr[Promise[Boolean]], result : Expr[scala.util.Try[Boolean]]) = {
     reify {
       if (result.splice.isFailure) {
         completePromise(promise, result).splice
       } else {
         elseExpr.splice
       }
     }
  }

  private def buildDef(symbol: Symbol, cases : List[CaseDef]) = DefDef(symbol, buildMatch(cases))

  private def buildMatch(cases : List[CaseDef]) = Match(Ident(TermName("state")), cases)

  private def mkResumeApply(symLookup: SymLookup) =
    Apply(symLookup.memberRef(TermName("resume")), Nil)


  def caseAwaitCall(state: Int, future: Expr[Future[Boolean]], fun: Expr[(scala.util.Try[Boolean]) => Unit]) = {
    cq"$state => ${onComplete(future, fun)}"
  }

  def onComplete(future: Expr[Future[Boolean]], fun: Expr[(scala.util.Try[Boolean]) => Unit]) = {
    reify {
      future.splice.onComplete(fun.splice)(scala.concurrent.ExecutionContext.global)
    }

  }

  def completePromise(promise: Expr[Promise[Boolean]], result: Expr[scala.util.Try[Boolean]]) = {
    reify {
      promise.splice.complete(result.splice);
      ()
    }
  }

  private def mkStateTree(nextState: Int, symLookup: SymLookup): Tree =
    Assign(symLookup.memberRef(TermName("state")), Literal(Constant(nextState)))


  def promType[A: WeakTypeTag]: Type = weakTypeOf[Promise[A]]
  def tryType[A: WeakTypeTag]: Type = weakTypeOf[scala.util.Try[A]]
  def execContextType: Type = weakTypeOf[ExecutionContext]

  def createProm[A: WeakTypeTag]: Expr[Promise[A]] = reify {
    Promise[A]()
  }

  def promiseToFuture[A: WeakTypeTag](prom: Expr[Promise[A]]) = reify {
    prom.splice.future
  }

  def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecutionContext]) = reify {
    Future(a.splice)(execContext.splice)
  }

  def onComplete[A, U](future: Expr[Future[A]], fun: Expr[scala.util.Try[A] => U],
                       execContext: Expr[ExecutionContext]): Expr[Unit] = reify {
    future.splice.onComplete(fun.splice)(execContext.splice)
  }

  def completeProm[A](prom: Expr[Promise[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
    prom.splice.complete(value.splice)
    Expr[Unit](Literal(Constant(()))).splice
  }

  def tryyIsFailure[A](tryy: Expr[scala.util.Try[A]]): Expr[Boolean] = reify {
    tryy.splice.isFailure
  }

  def tryyGet[A](tryy: Expr[scala.util.Try[A]]): Expr[A] = reify {
    tryy.splice.get
  }
  def tryySuccess[A: WeakTypeTag](a: Expr[A]): Expr[scala.util.Try[A]] = reify {
    scala.util.Success[A](a.splice)
  }
  def tryyFailure[A: WeakTypeTag](a: Expr[Throwable]): Expr[scala.util.Try[A]] = reify {
    scala.util.Failure[A](a.splice)
  }

  def stateMachineSkeleton() = {
    val tree = reify {
      class MyStateMachine extends AnyRef {
        var result$async : scala.concurrent.Promise[Boolean] = scala.concurrent.Promise.apply[Boolean]();
        var state : Int = 0;

        def resume$async() : Unit = {}

        def apply(result : scala.util.Try[Boolean]): Unit = {
          Unit
        }

      }
    }
    tree
  }


}

object Macros {

  @compileTimeOnly("`await` must be enclosed in an `async` block")
  def await(f: Future[Boolean]) : Boolean = ???

  def async(value: Boolean) : Future[Boolean] = macro AsyncMacroInigo.asyncImpl

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