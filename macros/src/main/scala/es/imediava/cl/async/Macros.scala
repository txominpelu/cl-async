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
      val result = stateMachineClass.info.member(name)
      result
    }

    def memberRef(name: TermName): Tree = {
      val result = gen.mkAttributedRef(stateMachineMember(name))
      result
    }
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

  def asyncImpl(value: c.Expr[Boolean])(execContext : c.Expr[ExecutionContext]) : c.Expr[Future[Boolean]] = {
    val execContextValDef = ValDef(NoMods, TermName("context"), TypeTree(), execContext.asInstanceOf[Expr[ExecutionContext]].tree)
    val skeleton = stateMachineSkeleton(applyDefDefDummyBody, execContextValDef).tree
    val classSkeleton = asyncMacro.callSiteTyper.typedPos(skeleton.pos)(skeleton).setType(skeleton.tpe).asInstanceOf[Block].stats.head
    // Create `ClassDef` of state machine with empty method bodies for `resume` and `apply`.
    println(s"Param symbol: ${applyDefDefDummyBody.vparamss.head.head.symbol}")
    val sLookup = SymLookup(classSkeleton.symbol, applyDefDefDummyBody.vparamss.head.head.symbol)


    val awaitCall = blockToList(value.tree.asInstanceOf[Tree]).collectFirst{
      case valDef @ ValDef (mods , _, tp, call @ Apply(method, param1 :: Nil)) if isAwait(call)   =>
        Awaitable(sLookup.memberRef("f1"), valDef.name)
    }

    val awaitState = new AsyncStateWithAwait(Nil, 0, 1, awaitCall.get, sLookup)
    val resumeAsyncDef = buildDef(sLookup.stateMachineMember("resume$async"), awaitState.mkHandlerCaseForState :: Nil)
    val applyDef = buildDef(sLookup.stateMachineMember("apply"), awaitState.mkOnCompleteHandler.get :: Nil)

    change(resumeAsyncDef.symbol, classSkeleton)
    change(applyDef.symbol, classSkeleton)

    val newClassSkeleton = asyncMacro.callSiteTyper.typedPos(classSkeleton.pos)(classSkeleton).setType(classSkeleton.tpe)
    val finalTree1 = asyncMacro.transformAt(newClassSkeleton){
      case dd@DefDef(_, TermName("resume$async"), _, List(_), _, _) if dd.symbol.owner == newClassSkeleton.symbol =>
        (ctx: analyzer.Context) =>
          treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, resumeAsyncDef.rhs)
    }
    val finalTree2 = asyncMacro.transformAt(finalTree1){
      case dd@DefDef(_, TermName("apply"), _, List(List(_)), _, _) if dd.symbol.owner == newClassSkeleton.symbol =>
        (ctx: analyzer.Context) =>
          treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, applyDef.rhs)
    }


    val finalTree3 = asyncMacro.transformAt(finalTree2){
      case th@This(a) if th.symbol.owner != classSkeleton.symbol.owner =>
        (ctx: analyzer.Context) =>
          println(showRaw(th))
          Select(This(classSkeleton.symbol), sLookup.stateMachineMember("apply"))
    }
    //println(s"Final Class=${showRaw(finalTree2)}")
    val myClass = finalTree3.asInstanceOf[ClassDef]

    def selectStateMachine(selection: TermName) = Select(Ident("stateMachine"), selection)

    val final3 = Block(List[Tree](
      myClass,
      ValDef(NoMods, TermName("stateMachine"), TypeTree(), Apply(Select(New(Ident(myClass.symbol)), nme.CONSTRUCTOR), Nil)),
      spawn(Apply(selectStateMachine(TermName("apply")), Nil), reify { scala.concurrent.ExecutionContext.global }.tree)
    ),
      promiseToFuture(Expr[Promise[Boolean]](selectStateMachine("result$async"))).tree)

    val final4 = asyncMacro.transformAt(final3){
      case mtch @ Match(id @Ident(t@TermName("state")), cases ) if id.symbol.owner != newClassSkeleton.symbol =>
        (ctx: analyzer.Context) =>
          val newMatch = Match(sLookup.memberRef(TermName("state")), cases )
          asyncMacro.callSiteTyper.typedPos(newMatch.pos)(newMatch)
          //println(s"New thing: ${show(newMatch, printTypes = true)}")
          //treeCopy.Match(newMatch, sLookup.memberRef(TermName("state")), cases)
    }



    val result = final4
    asyncMacro.callSiteTyper.typed(result).setType(typeOf[Future[Boolean]])

    println(s"Final Class=${show(result)}")
    println(s"Final Class=${showRaw(result, printIds = true)}")
    c.Expr[Future[Boolean]](result.asInstanceOf[c.Tree])

  }

  private def literalUnit = Literal(Constant(()))

  private def mkHandlerCase(num: Int, rhs: List[Tree]): CaseDef =
    mkHandlerCase(num, Block(rhs, literalUnit))

  private def mkHandlerCase(num: Int, rhs: Tree): CaseDef =
    CaseDef(Literal(Constant(num)), EmptyTree, rhs)

  case class Awaitable(expr: Tree, resultName: TermName)

  val applyDefDefDummyBody: DefDef = {
    val applyVParamss = List(List(ValDef(Modifiers(Flag.PARAM), TermName("result"), TypeTree(tryType[Boolean]), EmptyTree)))
    DefDef(NoMods, TermName("apply"), Nil, applyVParamss, TypeTree(definitions.UnitTpe), Literal(Constant(())))
  }

  trait AsyncState {
    def state: Int

    def nextStates: List[Int]

    def mkHandlerCaseForState: CaseDef

    def mkOnCompleteHandler: Option[CaseDef] = None

    var stats: List[Tree]

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
      //val callOnComplete = onComplete(Expr(awaitable.expr),Expr(This(tpnme.EMPTY))).tree
      val callOnComplete = onComplete(Expr(awaitable.expr),Expr(This(tpnme.EMPTY)), Expr(symLookup.memberRef("context"))).tree
      //val callOnComplete = onComplete(Expr(awaitable.expr),Expr(symLookup.memberRef("apply"))).tree
      //println(show(callOnComplete))
      mkHandlerCase(state, stats :+ callOnComplete)
    }

    override def mkOnCompleteHandler: Option[CaseDef] = {
      val tryGetTree =
        Assign(
          symLookup.memberRef(awaitable.resultName),
          TypeApply(Select(tryyGet[Boolean](Expr[scala.util.Try[Boolean]](Ident(symLookup.applyTrParam))).tree, newTermName("asInstanceOf")), List(TypeTree(definitions.BooleanTpe)))
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
        If(tryyIsFailure(Expr[scala.util.Try[Boolean]](Ident(symLookup.applyTrParam))).tree,
          completeProm[Boolean](
            Expr[Promise[Boolean]](symLookup.memberRef(TermName("result$async"))),
            Expr[scala.util.Try[Boolean]](
              TypeApply(Select(Ident(symLookup.applyTrParam), newTermName("asInstanceOf")),
                List(TypeTree(tryType[Boolean]))))).tree,
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

  private def buildDef(symbol: Symbol, cases : List[CaseDef]) = DefDef(symbol, buildMatch(cases)).setType(typeOf[Unit])

  private def buildMatch(cases : List[CaseDef]) = Match(Ident(TermName("state")), cases).setType(typeOf[Unit])

  private def mkResumeApply(symLookup: SymLookup) =
    Apply(symLookup.memberRef(TermName("resume$async")), Nil)


  def onComplete(future: Expr[Future[Boolean]], fun: Expr[(scala.util.Try[Boolean]) => Unit], context : Expr[ExecutionContext]) = {
    reify {
      future.splice.onComplete(fun.splice)(context.splice)
      ()
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

  def spawn(tree: Tree, execContext: Tree): Tree =
    future(Expr[Unit](tree))(Expr[ExecutionContext](execContext)).tree

  def promiseToFuture[A: WeakTypeTag](prom: Expr[Promise[A]]) = reify {
    prom.splice.future
  }

  def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecutionContext]) = reify {
    Future(a.splice)(execContext.splice)
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

  def stateMachineSkeleton(applyDef: DefDef, execContext : ValDef) = {
    val tree = reify {
      class MyStateMachine extends AnyRef {
        Expr[Unit](execContext).splice
        var result$async : scala.concurrent.Promise[Boolean] = scala.concurrent.Promise.apply[Boolean]();
        var state : Int = 0;
        var f1 : scala.concurrent.Future[Boolean] = null
        var a1 : scala.Boolean = false

        Expr[() => Unit ](applyDef).splice

        def resume$async() : Unit = {}

        def apply() : Unit = resume$async()

      }
    }
    tree
  }


}

object Macros {

  @compileTimeOnly("`await` must be enclosed in an `async` block")
  def await(f: Future[Boolean]) : Boolean = ???

  def async(value: Boolean)(implicit execContext : ExecutionContext) : Future[Boolean] = macro AsyncMacroInigo.asyncImpl

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