package es.imediava.cl.async

import language.experimental.macros

import scala.reflect.macros.BlackboxMacro
import scala.reflect.macros.BlackboxContext
import scala.concurrent.{ExecutionContext, Promise, Await, Future}
import scala.concurrent.duration.{Duration, DurationInt}
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

  object traverser extends Traverser {
       var applies = List[Apply]()
       override def traverse(tree: Tree): Unit = tree match {
         case a if a.symbol == NoSymbol =>
           println(s"No symbol at: $a")
           super.traverseTrees(a.children)
         case _ =>
           //println(s"Reading children of:$tree: ${tree.children}")
           super.traverseTrees(tree.children)
       }
     }


}

class OwnerRepair[C <: reflect.macros.BlackboxContext with Singleton](val c: C) {
  /**
   * If macro arguments are spliced into underneath DefTree that introduces
   * an entry into the symbol ownership chain, any symbols defined in the
   * spliced tree will be ill-owned.
   *
   * This method detects this situation, and corrects the owners.
   */
  def repairOwners[A](expr: c.Expr[A]): c.Expr[A] = {
    val symtab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val utils = new Utils[symtab.type](symtab)

    // Proactively typecheck the tree. This will assign symbols to
    // DefTrees introduced by the macro.
    val typed = c.typeCheck(expr.tree).asInstanceOf[symtab.Tree]

    // The current owner at the call site. Symbols owned by this may need
    // to be transplanted.
    import scala.reflect.macros.runtime.{Context => MRContext}
    val callsiteOwner =
      c.asInstanceOf[MRContext]
        .callsiteTyper.context.owner
        .asInstanceOf[symtab.Symbol]

    val repairedTree = utils.repairOwners(typed, callsiteOwner)
    c.Expr[A](repairedTree.asInstanceOf[c.universe.Tree])
  }

  private class Utils[U <: reflect.internal.SymbolTable](val u: U) {
    import u._

    class ChangeOwnerAndModuleClassTraverser(oldowner: Symbol, newowner: Symbol)
      extends ChangeOwnerTraverser(oldowner, newowner) {

      override def traverse(tree: Tree) {
        tree match {
          case _: DefTree => change(tree.symbol.moduleClass)
          case _          =>
        }
        super.traverse(tree)
      }
    }

    def repairOwners(t: Tree, macroCallSiteOwner: Symbol): Tree = {
      object repairer extends Transformer {
        override def transform(t: Tree): Tree = {
          // TODO see `fixerUpper` in the pattern matcher for a slightly simpler way to do this.
          if (currentOwner.hasTransOwner(macroCallSiteOwner) && currentOwner.owner != macroCallSiteOwner)
            new ChangeOwnerAndModuleClassTraverser(macroCallSiteOwner, currentOwner)(t)
          else super.transform(t)
        }
      }
      repairer transform t
    }
  }
}

trait AsyncMacroInigo extends BlackboxMacro {

  val stateGenerator = {
    var count = 0
    () => {
      count = count + 1
      count
    }
  }

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

    val awaitCalls = blockToList(value.tree.asInstanceOf[Tree]).collect{
      case valDef @ ValDef (mods , _, tp, call @ Apply(method, param1 :: Nil)) if isAwait(call)   =>
        valDef
    }


    val execContextValDef = ValDef(NoMods, TermName("context"), TypeTree(), execContext.asInstanceOf[Expr[ExecutionContext]].tree)
    val skeleton = stateMachineSkeleton(applyDefDefDummyBody, execContextValDef).tree

    val classSkeleton2 = asyncMacro.callSiteTyper.typedPos(skeleton.pos)(skeleton).setType(skeleton.tpe).asInstanceOf[Block].stats.head

    val awaitVarsNulledOut = blockToList(value.tree.asInstanceOf[Tree]).collect{
      case valDef @ ValDef (mods , name, tp, call @ Apply(method, param1 :: Nil)) if isAwait(call)   =>
        resetLocalAttrs(valDef)
        val newValDef = treeCopy.ValDef(valDef, NoMods, name, tp, gen.mkZero(typeOf[Boolean]))
        newValDef.symbol.owner = classSkeleton2.symbol
        classSkeleton2.symbol.info.decls.enter(newValDef.symbol)
        newValDef
      case valDef @ ValDef (mods , name, tp, call)  =>
        resetLocalAttrs(valDef)
        valDef.symbol.owner = classSkeleton2.symbol
        classSkeleton2.symbol.info.decls.enter(valDef.symbol)
        valDef
    }

    //val myRef = gen.mkAttributedRef(awaitVarsNulledOut.last.symbol)

    val awaitables = awaitCalls.collect{
      case valDef @ ValDef (mods , _, tp, call @ Apply(method, Ident(paramName:TermName) :: Nil)) if isAwait(call)   =>
        //println("My param")
        //println(showRaw(param))
        Awaitable(SymLookup(classSkeleton2.symbol, applyDefDefDummyBody.vparamss.head.head.symbol).memberRef(paramName), valDef.name)
    }

    val classSkeleton3 = asyncMacro.transformAt(classSkeleton2) {
      case t@Template(parents, self, stats) =>
        (ctx: analyzer.Context) => {
          treeCopy.Template(t, parents, self, awaitVarsNulledOut ++ stats)
        }
    }

    val classSkeleton = asyncMacro.callSiteTyper.typedPos(classSkeleton3.pos)(classSkeleton3).setType(classSkeleton3.tpe)




    // Create `ClassDef` of state machine with empty method bodies for `resume` and `apply`.
    val sLookup = SymLookup(classSkeleton.symbol, applyDefDefDummyBody.vparamss.head.head.symbol)


    val awaitStates = awaitables.zipWithIndex.map{
      case (awaitable: Awaitable, index: Int) =>
        new AsyncStateWithAwait(Nil, index, index + 1, awaitable, sLookup)
    }

    val casesForState = awaitStates.map(_.mkHandlerCaseForState)
    val onCompleteHandlers = awaitStates.map(_.mkOnCompleteHandler).flatten

    gen.stabilize(classSkeleton)
    println(show(classSkeleton))


    val finalState = new FinalAsyncState(awaitStates.last.nextState, Expr[Promise[Boolean]](sLookup.memberRef("result$async")), reify { scala.util.Success(Expr[Boolean](sLookup.memberRef(awaitables.last.resultName)).splice) })

    val resumeAsyncDef = buildDef(sLookup.stateMachineMember("resume$async"), finalState.mkHandlerCaseForState :: casesForState)
    val applyDef = buildDef(sLookup.stateMachineMember("apply"), onCompleteHandlers)

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
          Select(This(classSkeleton.symbol), sLookup.stateMachineMember("apply"))
    }

    val myClass = finalTree3.asInstanceOf[ClassDef]

    def selectStateMachine(selection: TermName) = Select(Ident("stateMachine"), selection)

    val typedClass = asyncMacro.callSiteTyper.typedPos(myClass.pos)(myClass)

    val final3 = Block(List[Tree](
      typedClass,
      ValDef(NoMods, TermName("stateMachine"), TypeTree(), Apply(Select(New(Ident(myClass.symbol)), nme.CONSTRUCTOR), Nil)),
      spawn(Apply(selectStateMachine(TermName("apply2")), Nil), reify { scala.concurrent.ExecutionContext.global }.tree)
    ),
      promiseToFuture(Expr[Promise[Boolean]](selectStateMachine("result$async"))).tree)

    val final4 = asyncMacro.transformAt(final3){
      case mtch @ Match(id @Ident(t@TermName("state")), cases ) if id.symbol.owner != newClassSkeleton.symbol =>
        (ctx: analyzer.Context) =>
          val newMatch = Match(sLookup.memberRef(TermName("state")), cases )
          asyncMacro.callSiteTyper.typedPos(newMatch.pos)(newMatch)
    }

    val final5 = asyncMacro.transformAt(final4) {
      case f@Function(_, _) if f.symbol.owner != newClassSkeleton =>
        (ctx: analyzer.Context) =>
          changeOwner(f, f.symbol.owner, ctx.owner)
    }

    val result = asyncMacro.transformAt(final5) {
      case f@ValDef(_, _, _, _) if f.symbol.owner != newClassSkeleton =>
        (ctx: analyzer.Context) =>
          changeOwner(f, f.symbol.owner, ctx.owner)
    }


    asyncMacro.callSiteTyper.typed(result).setType(typeOf[Future[Boolean]])

    // end of the macro impl
    val ownerRepair = new OwnerRepair[c.type](c)
    val checked = ownerRepair.repairOwners(c.Expr[Future[Boolean]](result.asInstanceOf[c.Tree])).tree

    println(s"Final Class=${show(result)}")
    println(s"Final Class=${showRaw(result, printTypes = true)}")
    c.Expr[Future[Boolean]](checked)

  }

  def changeOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol): tree.type = {
    new ChangeOwnerAndModuleClassTraverser(oldOwner, newOwner).traverse(tree)
    tree
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

  class ChangeOwnerAndModuleClassTraverser(oldowner: Symbol, newowner: Symbol)
    extends ChangeOwnerTraverser(oldowner, newowner) {

    override def traverse(tree: Tree) {
      tree match {
        case _: DefTree => change(tree.symbol.moduleClass)
        case _          =>
      }
      super.traverse(tree)
    }
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

  case class FinalAsyncState(state: Int, promise: Expr[Promise[Boolean]], result: Expr[scala.util.Try[Boolean]]) extends AsyncState {

    def nextStates: List[Int] = Nil

    override def mkHandlerCaseForState: CaseDef = cq"${state} => ${completePromise(promise, result)}".asInstanceOf[CaseDef]

    var stats: List[Tree] = Nil

  }


  final class AsyncStateWithAwait(var stats: List[Tree], val state: Int, val nextState: Int,
                                  val awaitable: Awaitable, symLookup: SymLookup)
    extends AsyncState {

    def nextStates: List[Int] =
      List(nextState)

    override def mkHandlerCaseForState: CaseDef = {
      val callOnComplete = onComplete(Expr(awaitable.expr),Expr(This(tpnme.EMPTY)), Expr(symLookup.memberRef("context")))
      mkHandlerCase(state, stats :+ callOnComplete.tree)
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

  private def buildDef(symbol: Symbol, cases : List[CaseDef]) = DefDef(symbol, buildMatch(cases)).setType(typeOf[Unit])

  private def buildMatch(cases : List[CaseDef]) = Match(Ident(TermName("state")), cases).setType(typeOf[Unit])

  private def mkResumeApply(symLookup: SymLookup) =
    Apply(symLookup.memberRef(TermName("resume$async")), Nil)


  def onComplete(future: Expr[Future[Boolean]], fun: Expr[(scala.util.Try[Boolean]) => Unit], context: Expr[ExecutionContext]) = {
    //Apply(Apply(TypeApply(Select(Ident(sLookup.stateMachineMember("f1")),TermName("onComplete")), List(TypeTree(typeOf[Unit]))),
      //List(This(TypeName("MyStateMachine")))), List(Select(This(sLookup.stateMachineClass), TermName("context"))))
    reify {
      future.splice.onComplete(fun.splice)(context.splice)
      ()
    }

  }

  def completePromise(promise: Expr[Promise[Boolean]], result: Expr[scala.util.Try[Boolean]]) = {
    q"${promise}.complete(${result})"
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
      class MyStateMachine extends (scala.util.Try[Boolean] => Unit) {
        Expr[Unit](execContext).splice
        var result$async : scala.concurrent.Promise[Boolean] = scala.concurrent.Promise.apply[Boolean]();
        var state : Int = 0;

        Expr[(scala.util.Try[Boolean]) => Unit ](applyDef).splice

        def resume$async() : Unit = {}

        def apply2() : Unit = resume$async()

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