package rescala.macros

import rescala.core.{CreationTicket, DynamicTicket, LowPriorityCreationImplicits, StaticTicket, Struct}
import retypecheck._

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

object MacroTags {
  type Staticism
  type Static <: Staticism
  type Dynamic <: Staticism
}

/** Annotated definitions are cut out of any reactive macro and only evaluated once when the reactive is created.
  * This causes more dependencies to be static and reduces the number of unnecessarily created reactives. */
class cutOutOfUserComputation extends StaticAnnotation

class ReactiveMacros(val c: blackbox.Context) {

  import c.universe._

  private def compileErrorsAst: Tree =
    q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

  def ReactiveExpression[
    A: c.WeakTypeTag,
    S <: Struct : c.WeakTypeTag,
    IsStatic <: MacroTags.Staticism : c.WeakTypeTag,
    ReactiveType : c.WeakTypeTag]
  (expression: Tree)(ticket: c.Tree): c.Tree = {
    ReactiveExpressionWithAPI[A, S, IsStatic, ReactiveType](expression)(ticket)(q"(${c.prefix.tree}).rescalaAPI", None)
  }

  def ReactiveExpressionWithAPI[
    A: c.WeakTypeTag,
    S <: Struct : c.WeakTypeTag,
    IsStatic <: MacroTags.Staticism : c.WeakTypeTag,
    ReactiveType : c.WeakTypeTag]
  (expression: Tree)(ticket: c.Tree)(rescalaAPI: Tree, prefixManipulation: Option[PrefixManipulation]): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val forceStatic = !(weakTypeOf[IsStatic] <:< weakTypeOf[MacroTags.Dynamic])
    val lego = new MacroLego(expression, forceStatic)

    val signalsOrEvents = weakTypeOf[ReactiveType].typeSymbol.asClass.module.asTerm.name
    val dependencies = lego.detections.detectedStaticReactives
    val isStatic = lego.detections.detectedDynamicReactives.isEmpty
    val creationMethod = TermName(if (isStatic) "static" else "dynamic")
    val ticketType = if (isStatic) weakTypeOf[StaticTicket[S]] else weakTypeOf[DynamicTicket[S]]

    val body = q"""$rescalaAPI.$signalsOrEvents.$creationMethod[${weakTypeOf[A]}](
         ..$dependencies
         ){${lego.contextualizedExpression(ticketType)}}($ticket)"""

    lego.wrapFinalize(body, prefixManipulation)
  }

  def UDFExpressionWithAPI[
    T: c.WeakTypeTag,
    DependencyType: c.WeakTypeTag,
    Capability: c.WeakTypeTag]
  (expression: Tree): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val forceStatic = !(weakTypeOf[Capability] <:< weakTypeOf[DynamicTicket[_]])
    val lego = new MacroLego(expression, forceStatic)

    val dependencies = lego.detections.detectedStaticReactives
    val isStatic = lego.detections.detectedDynamicReactives.isEmpty
    val ticketType = weakTypeOf[Capability]

    val body = q"""_root_.rescala.reactives.UserDefinedFunction[${weakTypeOf[T]}, ${weakTypeOf[DependencyType]}, ${ticketType}](
         _root_.scala.collection.immutable.Set[${weakTypeOf[DependencyType]}](..$dependencies),
         ${lego.contextualizedExpression(ticketType)},
         ${isStatic}
         )"""

    lego.wrapFinalize(body, None)
  }


  def fixNullTypes(tree: Tree): Unit =
    tree.foreach(t => if (t.tpe == null) internal.setType(t, NoType))


  object ForceCutOut
  class PrefixManipulation {
    private val prefixTermName: TermName = TermName(c.freshName("prefix$"))
    val prefixIdent: Ident = Ident(prefixTermName)
    def prefixValue: Tree = {
      //val prefixTree = c.prefix.tree
      internal.updateAttachment(prefixIdent, IsCutOut)
      internal.setType(prefixIdent, c.prefix.actualType)
      q"$prefixIdent.value"
    }
    def prefixValDef: ValDef = {
      q"val $prefixTermName : ${c.prefix.actualType} = ${c.prefix.tree}": ValDef
    }
  }


  def ReactiveUsingFunctionMacro[
    T: c.WeakTypeTag,
    A: c.WeakTypeTag,
    S <: Struct : c.WeakTypeTag,
    FuncImpl: c.WeakTypeTag,
    ReactiveType : c.WeakTypeTag]
  (expression: c.Tree)(ticket: c.Tree): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val funcImpl = weakTypeOf[FuncImpl].typeSymbol.asClass.module
    val pm = new PrefixManipulation
    val computation: Tree = q"""$funcImpl.apply[${weakTypeOf[T]}, ${weakTypeOf[A]}](${pm.prefixValue}, $expression)"""
    fixNullTypes(computation)
    ReactiveExpressionWithAPI[A, S, MacroTags.Dynamic, ReactiveType](computation)(ticket)(q"${pm.prefixIdent}.rescalaAPI", Some(pm))
  }


  def EventFoldMacro[T: c.WeakTypeTag, A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (init: c.Expr[A])
  (op: c.Expr[(A, T) => A])
  (ticket: c.Expr[rescala.core.CreationTicket[S]])
  : c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val eventsSymbol = weakTypeOf[rescala.reactives.Events.type].termSymbol.asTerm.name
    val ticketType = weakTypeOf[StaticTicket[S]]
    val funcImpl = weakTypeOf[rescala.reactives.Events.FoldFuncImpl.type].typeSymbol.asClass.module
    val pm = new PrefixManipulation()
    val computation = q"""$funcImpl.apply[${weakTypeOf[T]}, ${weakTypeOf[A]}](_, ${pm.prefixValue}, $op)"""
    fixNullTypes(computation)

    val lego = new MacroLego(computation, forceStatic = true)
    val detections = lego.detections.detectedStaticReactives

    val body =
      q"""${pm.prefixIdent}.rescalaAPI.$eventsSymbol.fold[${weakTypeOf[A]}](Set(..$detections), $init)(
           ${lego.contextualizedExpression(ticketType)}
          )($ticket)"""

    lego.wrapFinalize(body, Some(pm))
  }




  // here be dragons


  class MacroLego[S <: Struct: c.WeakTypeTag](tree: Tree, forceStatic: Boolean) {
    private val ticketTermName: TermName = TermName(c.freshName("ticket$"))
    val ticketIdent: Ident = Ident(ticketTermName)

    val weAnalysis = new WholeExpressionAnalysis(tree)
    val cutOut = new CutOutTransformer(weAnalysis)
    val cutOutTree = cutOut.transform(tree)
    val detections = new RewriteTransformer[S](weAnalysis, ticketIdent, forceStatic)
    val rewrittenTree = detections transform cutOutTree

    def contextualizedExpression(contextType: Type) =
      q"{$ticketTermName: $contextType => $rewrittenTree }"

    def wrapFinalize(body: Tree, prefixManipulation: Option[PrefixManipulation]): Tree = {
      val valDefs = prefixManipulation.map(_.prefixValDef).toList ::: cutOut.cutOutReactivesVals.reverse
      val block: c.universe.Tree = Block(valDefs, body)
      ReTyper(c).untypecheck(block)
    }

  }

  object IsCutOut

  class RewriteTransformer[S <: Struct : WeakTypeTag](weAnalysis: WholeExpressionAnalysis, ticketIdent: Ident, requireStatic: Boolean) extends Transformer {

    // list of detected inner signals
    var detectedDynamicReactives = List[Tree]()
    var detectedStaticReactives = List[Tree]()

    override def transform(tree: Tree): Tree =
      tree match {
        // replace any used CreationTicket in a Signal expression with the correct turn source for the current turn
        //q"$_.fromSchedulerImplicit[..$_](...$_)"
        case turnSource@Apply(TypeApply(Select(_, TermName("fromSchedulerImplicit")), _), _)
          if turnSource.tpe =:= weakTypeOf[CreationTicket[S]] && turnSource.symbol.owner == symbolOf[LowPriorityCreationImplicits] =>
          q"""${termNames.ROOTPKG}.rescala.core.CreationTicket(
                  ${termNames.ROOTPKG}.scala.Left($ticketIdent.initializer),
                  ${termNames.ROOTPKG}.rescala.core.REName.create)"""

        case tree@Select(reactive, TermName("now")) =>
          c.warning(tree.pos, "Using `now` inside a reactive expression does not create a dependency, " +
            "and can result in glitches. Use `apply` instead.")
          super.transform(tree)
        // Access every reactive through the ticket argument
        // to obtain dynamic dependencies
        //
        // for example, this step transforms
        //   Signal { reactive_1() + reactive_2() }
        // to
        //   Signals.dynamic { s => s.depend(reactive_1) + s.depend(reactive_2) }
        case tree@MacroInterpretable(untransformedReactive) =>
          val reactive = transform(untransformedReactive)
          val reactiveApply =
            if (weAnalysis.isStaticDependency(reactive)) {
              detectedStaticReactives ::= reactive
              q"$ticketIdent.dependStatic"
            }
            else {
              detectedDynamicReactives ::= reactive
              if (requireStatic) c.error(tree.pos, "access to reactive may be dynamic")
              q"$ticketIdent.depend"
            }
          internal.setType(reactiveApply, tree.tpe)
          q"$reactiveApply($reactive.resource)"
        case _ =>
          super.transform(tree)
      }
  }

  class CutOutTransformer(weAnalysis: WholeExpressionAnalysis) extends Transformer {

    // the signal values that will be cut out of the Signal expression
    var cutOutReactivesVals = List[ValDef]()
    var cutOutReactiveIdentifiers = List[Tree]()

    override def transform(tree: Tree): Tree =
      tree match {
        // cut signal values out of the signal expression, that could
        // potentially create a new signal object for every access
        // it is assumed that such functions are pure in the sense that they
        // will create an equivalent signal for each call with the same
        // arguments so the function value has to be calculated just once
        //
        // for example, this step transforms
        //   Signal { event.count.apply() }
        // to
        // {
        //   val s = event.count
        //   Signal { s() }
        // }
        //
        case reactive
          if weAnalysis.isReactiveThatCanBeCutOut(reactive) =>

          // create the signal definition to be cut out of the
          // macro expression and its substitution variable
          val cutOutReactiveTermName = TermName(c.freshName("reactive$"))
          val ident = Ident(cutOutReactiveTermName)
          val signalDef: ValDef = q"val $cutOutReactiveTermName = $reactive"

          cutOutReactivesVals ::= signalDef
          cutOutReactiveIdentifiers ::= ident

          internal.updateAttachment(ident, IsCutOut)
          internal.setType(ident, reactive.tpe)
          ident

        case _ =>
          super.transform(tree)
      }
  }


  class WholeExpressionAnalysis(expression: Tree) {
    val uncheckedExpressions: Set[Tree] = calcUncheckedExpressions(expression)

    // all symbols that are defined within the macro expression
    val symbolsDefinedInsideMacroExpression: Map[Symbol, DefTree] = (expression collect {
      // filter NoSymbols, as everything we synthetically generate seems to miss the symbol,
      // resulting in synthetic reactive accesses potentially not being cut out
      case defTree: DefTree if defTree.symbol != NoSymbol => defTree.symbol -> defTree
    }).toMap


    def checkForPotentialSideEffects(): Unit = ReactiveMacros.this.checkForPotentialSideEffects(expression, uncheckedExpressions)

    /** detects reactives which are guaranteed to be accessed statically inside the macro */
    def isStaticDependency(tree: c.universe.Tree): Boolean = tree.forAll { t =>
      !symbolsDefinedInsideMacroExpression.contains(t.symbol) &&
        (internal.attachments(t).contains[IsCutOut.type]
          || (t.symbol != null
          && (t.symbol.isType
          || (t.symbol.isTerm && t.symbol.asTerm.isStable))))
    }
    /** - is not a reactive value resulting from a function that is
      *   itself called on a reactive value
      *   (so the expression does not contained "chained" reactives)
      * - does not reference definitions that are defined within the
      *   macro expression but not within e
      *   (note that such a case can lead to unintentional behavior) */
    def containsCriticalReferences(reactive: c.universe.Tree): Boolean = reactive.filter {
      // check if reactive results from a function that is
      // itself called on a reactive value
      case MacroInterpretable(_) => true
      // check reference definitions that are defined within the
      // macro expression but not within the reactive
      case tree: SymTree =>
        symbolsDefinedInsideMacroExpression get tree.symbol match {
          case Some(defTree) => !(reactive exists {_ == defTree})
          case _ => false
        }

      // "uncritical" reactive that can be cut out
      case _ => false
    }.nonEmpty

    def isReactiveThatCanBeCutOut(outerReactive: c.universe.Tree): Boolean = {
      @scala.annotation.tailrec
      def annotatedForCutOut(reactive: c.universe.Tree): Boolean = reactive match {
        case Block(_, expr) => annotatedForCutOut(expr)
        case Typed(expr, _) => annotatedForCutOut(expr)
        case _ if internal.attachments(reactive).contains[ForceCutOut.type] => true
        case _ => if (reactive.symbol == null) false else {
          val directAnnotations = reactive.symbol.annotations
          val accessorAnnotations: List[Annotation] =
            if (reactive.symbol.isMethod && reactive.symbol.asMethod.isAccessor)
              reactive.symbol.asMethod.accessed.annotations
            else Nil

          (directAnnotations ++ accessorAnnotations) exists {_.tree.tpe <:< typeOf[cutOutOfUserComputation]}
        }
      }

      isInterpretable(outerReactive) && annotatedForCutOut(outerReactive) && !containsCriticalReferences(outerReactive)
    }
  }


  def treeTypeNullWarning(tree: Tree): Unit =
    c.warning(c.enclosingPosition,
      "internal warning: tree type was null, " +
        "this should not happen but the signal may still work\n" +
        s"Offending tree: $tree")

  def isInterpretable(tree: Tree): Boolean = {
    val staticInterpClass = c.mirror staticClass "_root_.rescala.core.MacroAccess"

    if (tree.tpe == null) {treeTypeNullWarning(tree); false}
    else
      !(tree.tpe <:< definitions.NullTpe) &&
        !(tree.tpe <:< definitions.NothingTpe) &&
        (tree.tpe.baseClasses contains staticInterpClass)
  }

  /** detects variants to access reactives using [[rescala.core.MacroAccess]] */
  object MacroInterpretable {
    def unapply(arg: Tree): Option[Tree] = arg match {
      case Apply(Select(reactive, tn), Nil)
        if "apply" == tn.decodedName.toString && isInterpretable(reactive) =>
        Some(reactive)
      case Select(reactive, tn)
        if "value" == tn.decodedName.toString && isInterpretable(reactive) =>
        Some(reactive)
      case _ => None
    }
  }

  /** find expressions annotated with @scala.annotation.unchecked  */
  def calcUncheckedExpressions[A: c.WeakTypeTag](expression: Tree): Set[c.universe.Tree] = {
    (expression collect {
      case tree@Typed(_, _)
        if (tree.tpe match {
          case AnnotatedType(annotations, _) =>
            annotations exists { _.tree.tpe <:< typeOf[unchecked] }
          case _ => false
        }) =>
        def uncheckedSubExpressions(tree: Tree): List[Tree] = tree match {
          case Select(expr, _) => expr :: uncheckedSubExpressions(expr)
          case Apply(expr, _) => expr :: uncheckedSubExpressions(expr)
          case TypeApply(expr, _) => expr :: uncheckedSubExpressions(expr)
          case Typed(expr, _) => expr :: uncheckedSubExpressions(expr)
          case Block(_, expr) => expr :: Nil
          case _ => Nil
        }
        uncheckedSubExpressions(tree)
    }).flatten.toSet
  }

  def checkForPotentialSideEffects[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
    (expression: Tree, uncheckedExpressions: Set[c.universe.Tree]): Unit = {
    // collect expression annotated to be unchecked and do not issue warnings
    // for those (use the standard Scala unchecked annotation for that purpose)

    // generate warning for some common cases where called functions are
    // either unnecessary (if free of side effects) or have side effects
    def isMethodWithPotentialNonLocalSideEffects(toCheck: Tree) = toCheck match {
      case function@(TypeApply(_, _) | Apply(_, _) | Select(_, _))
        if !(uncheckedExpressions contains function) =>
        val arguments = toCheck match {
          case TypeApply(_, args) => args
          case Apply(_, args) => args
          case _ => List.empty
        }

        val noFunctionInArgs = !(arguments exists {
          case tree
            if (tree.tpe match {
              case TypeRef(_, _, args) => args.nonEmpty
              case _ => false
            }) => true
          case _ => false
        })

        val noConstructorInFun = function exists {
          case Apply(fun, args) =>
            !(fun exists {
              case Select(_, termNames.CONSTRUCTOR) => true
              case _ => false
            })
          case _ => false
        }

        noFunctionInArgs && noConstructorInFun
      case _ => false
    }

    def potentialSideEffectWarning(pos: Position) =
      c.warning(pos,
        "Statement may either be unnecessary or have side effects. " +
          "Signal expressions should have no side effects.")

    expression foreach {
      case Block(stats, _) =>
        stats foreach { stat =>
          if (isMethodWithPotentialNonLocalSideEffects(stat))
            potentialSideEffectWarning(stat.pos)
        }
      case tree =>
        if (isMethodWithPotentialNonLocalSideEffects(tree) &&
          tree.tpe != null &&
          tree.tpe =:= typeOf[Unit])
          potentialSideEffectWarning(tree.pos)
    }
  }

}
