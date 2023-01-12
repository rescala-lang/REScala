package rescala.macros

import rescala.operator.cutOutOfUserComputation
import retypecheck._

import scala.reflect.macros.blackbox

object MacroTags {
  type Staticism
  type Static <: Staticism
  type Dynamic <: Staticism
}

class ReactiveMacros(val c: blackbox.Context) {

  import c.universe._

  private def compileErrorsAst: Tree =
    q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

  def ReactiveExpression[
      A: c.WeakTypeTag,
      IsStatic <: MacroTags.Staticism: c.WeakTypeTag,
      ReactiveType: c.WeakTypeTag,
      StaticTicket: c.WeakTypeTag,
      DynamicTicket: c.WeakTypeTag,
      ScopeSearch: c.WeakTypeTag,
      LowPriorityImplicitObject: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](expression: Tree)(ticket: c.Tree): c.Tree = {
    ReactiveExpressionWithAPI[
      A,
      IsStatic,
      ReactiveType,
      StaticTicket,
      DynamicTicket,
      ScopeSearch,
      LowPriorityImplicitObject,
      ResourceType
    ](expression)(ticket)(None)
  }

  def ReactiveExpressionWithAPI[
      A: c.WeakTypeTag,
      IsStatic <: MacroTags.Staticism: c.WeakTypeTag,
      ReactiveType: c.WeakTypeTag,
      StaticTicket: c.WeakTypeTag,
      DynamicTicket: c.WeakTypeTag,
      ScopeSearch: c.WeakTypeTag,
      LowPriorityImplicitObject: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](expression: Tree)(ticket: c.Tree)(prefixManipulation: Option[PrefixManipulation]): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val forceStatic = !(weakTypeOf[IsStatic] <:< weakTypeOf[MacroTags.Dynamic])
    val lego        = new MacroLego[ScopeSearch, LowPriorityImplicitObject, ResourceType](expression, forceStatic)

    val dependencies   = lego.detections.detectedStaticReactives
    val isStatic       = lego.detections.detectedDynamicReactives.isEmpty
    val creationMethod = TermName(if (isStatic) "static" else "dynamic")
    val ticketType     = if (isStatic) weakTypeOf[StaticTicket] else weakTypeOf[DynamicTicket]

    val wrt = weakTypeOf[ReactiveType]

    val resolved = wrt.asSeenFrom(new BundleAcquisiton[ResourceType].getBundle, wrt.typeSymbol.owner)
    val tq"$resolvedTree.$tpname.type" = ReTyper(c).createTypeTree(resolved, c.enclosingPosition)

    val body = q"""$resolvedTree.${tpname.toTermName}.$creationMethod[${weakTypeOf[A]}](
         ..$dependencies
         ){${lego.contextualizedExpression(ticketType)}}($ticket)"""

    lego.wrapFinalize(body, prefixManipulation)
  }

  def UDFExpressionWithAPI[
      T: c.WeakTypeTag,
      DependencyType: c.WeakTypeTag,
      Capability: c.WeakTypeTag,
      DynamicTicket: c.WeakTypeTag,
      ScopeSearch: c.WeakTypeTag,
      LowPriorityImplicitObject: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](expression: Tree): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val forceStatic = !(weakTypeOf[Capability] <:< weakTypeOf[DynamicTicket])
    val lego        = new MacroLego[ScopeSearch, LowPriorityImplicitObject, ResourceType](expression, forceStatic)

    val dependencies = lego.detections.detectedStaticReactives
    val isStatic     = lego.detections.detectedDynamicReactives.isEmpty
    val ticketType   = weakTypeOf[Capability]

    val body =
      q"""new UserDefinedFunction[${weakTypeOf[T]}, ${weakTypeOf[DependencyType]}, ${ticketType}](
         _root_.scala.collection.immutable.Set[${weakTypeOf[DependencyType]}](..$dependencies),
         ${lego.contextualizedExpression(ticketType)},
         ${isStatic}
         )"""

    lego.wrapFinalize(body, None)
  }

  def fixNullTypes(tree: Tree): Unit =
    tree.foreach(t => if (t.tpe == null) { internal.setType(t, NoType); () })

  object ForceCutOut
  class PrefixManipulation {
    private val prefixTermName: TermName = TermName(c.freshName("prefix$"))
    val prefixIdent: Ident               = Ident(prefixTermName)
    def prefixValue: Tree = {
      // val prefixTree = c.prefix.tree
      internal.updateAttachment(prefixIdent, IsCutOut)
      internal.setType(prefixIdent, c.prefix.actualType)
      q"$prefixIdent.value"
    }
    def prefixValDef: ValDef = {
      val prefixTypeTree = ReTyper(c).createTypeTree(c.prefix.actualType, c.enclosingPosition)
      q"val $prefixTermName: $prefixTypeTree = ${c.prefix.tree}"
    }
  }

  def ReactiveUsingFunctionMacro[
      T: c.WeakTypeTag,
      A: c.WeakTypeTag,
      FuncImpl: c.WeakTypeTag,
      ReactiveType: c.WeakTypeTag,
      StaticTicket: c.WeakTypeTag,
      DynamicTicket: c.WeakTypeTag,
      ScopeSearch: c.WeakTypeTag,
      LowPriorityImplicitObject: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](expression: c.Tree)(ticket: c.Tree): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val funcImpl          = weakTypeOf[FuncImpl].typeSymbol.asClass.module
    val pm                = new PrefixManipulation
    val computation: Tree = q"""$funcImpl.apply[${weakTypeOf[T]}, ${weakTypeOf[A]}](${pm.prefixValue}, $expression)"""
    fixNullTypes(computation)
    ReactiveExpressionWithAPI[
      A,
      MacroTags.Dynamic,
      ReactiveType,
      StaticTicket,
      DynamicTicket,
      ScopeSearch,
      LowPriorityImplicitObject,
      ResourceType
    ](computation)(ticket)(Some(pm))
  }

  def EventFoldMacro[
      T: c.WeakTypeTag,
      A: c.WeakTypeTag,
      FoldFunctionImpl: c.WeakTypeTag,
      ReactiveType: c.WeakTypeTag,
      CT,
      StaticTicket: c.WeakTypeTag,
      ScopeSearch: c.WeakTypeTag,
      LowPriorityImplicitObject: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](
      init: c.Expr[A]
  )(op: c.Expr[(A, T) => A])(ticket: c.Expr[CT]): c.Tree = {
    if (c.hasErrors) return compileErrorsAst

    val ticketType  = weakTypeOf[StaticTicket]
    val funcImpl    = weakTypeOf[FoldFunctionImpl].typeSymbol.asClass.module
    val pm          = new PrefixManipulation()
    val computation = q"""$funcImpl.apply[${weakTypeOf[T]}, ${weakTypeOf[A]}](_, ${pm.prefixValue}, $op)"""
    fixNullTypes(computation)

    val lego = new MacroLego[ScopeSearch, LowPriorityImplicitObject, ResourceType](computation, forceStatic = true)
    val detections = lego.detections.detectedStaticReactives

    val wrt = weakTypeOf[ReactiveType]

    val resolved = wrt.asSeenFrom(new BundleAcquisiton[ResourceType].getBundle, wrt.typeSymbol.owner)
    val tq"$resolvedTree.$tpname.type" = ReTyper(c).createTypeTree(resolved, c.enclosingPosition)

    val body = q"""$resolvedTree.${tpname.toTermName}.fold[${weakTypeOf[A]}](Set(..$detections), $init)(
           ${lego.contextualizedExpression(ticketType)}
          )($ticket)"""

    lego.wrapFinalize(body, Some(pm))
  }

  // here be dragons

  class BundleAcquisiton[ResourceType: c.WeakTypeTag] {

    val reSourceType: c.universe.Type = weakTypeOf[ResourceType]

    def underlying(tpe: Type): Type =
      if (tpe ne tpe.dealias)
        underlying(tpe.dealias)
      else if (tpe ne tpe.widen)
        underlying(tpe.widen)
      else
        tpe

    def underlyingReSourceType(tpe: Type): Type =
      underlying(tpe) match {
        case RefinedType(parents, _) =>
          underlyingReSourceType(parents.find(_ <:< reSourceType).get)
        case tpe =>
          tpe
      }

    def getBundle: Type = {
      val ntype = c.prefix.tree.tpe
      underlyingReSourceType(ntype) match {
        case TypeRef(tn, _, _) => tn: Type
        case _                 => throw new IllegalStateException(s"underlying type is not a type ref: $ntype")
      }
    }

    def getBundledClass(tpe: Type) = {

      def doMagic(tpe: Type) = {
        val tn = getBundle
        tpe.asSeenFrom(tn, tpe.typeSymbol.owner).dealias
      }

      def getMember(name: String) = {
        val tn = getBundle
        tn.member(TypeName(name))
      }

      doMagic(getMember(tpe.typeSymbol.name.toString).asType.toType)
    }
  }

  class MacroLego[
      CreationTicket: c.WeakTypeTag,
      LowPriorityCreationImplicits: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](
      tree: Tree,
      forceStatic: Boolean
  ) {
    private val ticketTermName: TermName = TermName(c.freshName("ticket$"))
    val ticketIdent: Ident               = Ident(ticketTermName)

    val weAnalysis = new WholeExpressionAnalysis(tree)
    val cutOut     = new CutOutTransformer(weAnalysis)
    val cutOutTree = cutOut.transform(tree)
    val detections =
      new RewriteTransformer[CreationTicket, LowPriorityCreationImplicits, ResourceType](
        weAnalysis,
        ticketIdent,
        forceStatic
      )
    val rewrittenTree = detections transform cutOutTree

    def contextualizedExpression(contextType: Type) = {

      q"{$ticketTermName: ${new BundleAcquisiton[ResourceType].getBundledClass(contextType)} => $rewrittenTree }"
    }

    def wrapFinalize(body: Tree, prefixManipulation: Option[PrefixManipulation]): Tree = {
      val valDefs                = prefixManipulation.map(_.prefixValDef).toList ::: cutOut.cutOutReactivesVals.reverse
      val block: c.universe.Tree = Block(valDefs, body)
      ReTyper(c).untypecheck(block)
    }

  }

  object IsCutOut

  class RewriteTransformer[
      CreationTicket: c.WeakTypeTag,
      LowPriorityCreationImplicits: c.WeakTypeTag,
      ResourceType: c.WeakTypeTag
  ](
      weAnalysis: WholeExpressionAnalysis,
      ticketIdent: Ident,
      requireStatic: Boolean
  ) extends Transformer {

    // list of detected inner signals
    var detectedDynamicReactives = List[Tree]()
    var detectedStaticReactives  = List[Tree]()

    override def transform(tree: Tree): Tree =
      tree match {
        // replace any used CreationTicket in a Signal expression with the correct turn source for the current turn
        // q"$_.fromSchedulerImplicit[..$_](...$_)"
        // TODO: this was disabled because of the bundle hack, need to figure out how to access creation ticket again
        case turnSource @ Apply(Select(ctleft, TermName("fromSchedulerImplicit")), _)
            if turnSource.tpe.typeSymbol.name == weakTypeOf[CreationTicket].typeSymbol.name
            && turnSource.symbol.owner == symbolOf[LowPriorityCreationImplicits] =>
          val ct = new BundleAcquisiton[ResourceType].getBundledClass(weakTypeOf[CreationTicket])
          q"""new $ct(${termNames.ROOTPKG}.scala.Left($ticketIdent.tx))"""

        case tree @ Select(reactive, TermName("now")) =>
          c.warning(
            tree.pos,
            "Using `now` inside a reactive expression does not create a dependency, " +
            "and can result in glitches. Use `value` instead."
          )
          super.transform(tree)
        // Access every reactive through the ticket argument
        // to obtain dynamic dependencies
        //
        // for example, this step transforms
        //   Signal { reactive_1() + reactive_2() }
        // to
        //   Signals.dynamic { s => s.depend(reactive_1) + s.depend(reactive_2) }
        case tree @ MacroInterpretable(untransformedReactive) =>
          val reactive = transform(untransformedReactive)
          val reactiveApply =
            if (weAnalysis.isStaticDependency(reactive)) {
              detectedStaticReactives ::= reactive
              q"$ticketIdent.dependStatic"
            } else {
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
    var cutOutReactivesVals       = List[ValDef]()
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
        case reactive if weAnalysis.isReactiveThatCanBeCutOut(reactive) =>
          // create the signal definition to be cut out of the
          // macro expression and its substitution variable
          val cutOutReactiveTermName = TermName(c.freshName("reactive$"))
          val ident                  = Ident(cutOutReactiveTermName)
          val signalDef: ValDef      = q"val $cutOutReactiveTermName = $reactive"

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

    def checkForPotentialSideEffects(): Unit =
      ReactiveMacros.this.checkForPotentialSideEffects(expression, uncheckedExpressions)

    /** detects reactives which are guaranteed to be accessed statically inside the macro */
    def isStaticDependency(tree: c.universe.Tree): Boolean =
      tree.forAll { t =>
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
      *   (note that such a case can lead to unintentional behavior)
      */
    def containsCriticalReferences(reactive: c.universe.Tree): Boolean =
      reactive.filter {
        // check if reactive results from a function that is
        // itself called on a reactive value
        case MacroInterpretable(_) => true
        // check reference definitions that are defined within the
        // macro expression but not within the reactive
        case tree: SymTree =>
          symbolsDefinedInsideMacroExpression get tree.symbol match {
            case Some(defTree) => !(reactive exists { _ == defTree })
            case _             => false
          }

        // "uncritical" reactive that can be cut out
        case _ => false
      }.nonEmpty

    def isReactiveThatCanBeCutOut(outerReactive: c.universe.Tree): Boolean = {
      @scala.annotation.tailrec
      def annotatedForCutOut(reactive: c.universe.Tree): Boolean =
        reactive match {
          case Block(_, expr)                                                 => annotatedForCutOut(expr)
          case Typed(expr, _)                                                 => annotatedForCutOut(expr)
          case _ if internal.attachments(reactive).contains[ForceCutOut.type] => true
          case _ =>
            if (reactive.symbol == null) false
            else {
              val directAnnotations = reactive.symbol.annotations
              val accessorAnnotations: List[Annotation] =
                if (reactive.symbol.isMethod && reactive.symbol.asMethod.isAccessor)
                  reactive.symbol.asMethod.accessed.annotations
                else Nil

              (directAnnotations ++ accessorAnnotations) exists { _.tree.tpe <:< typeOf[cutOutOfUserComputation] }
            }
        }

      isInterpretable(outerReactive) && annotatedForCutOut(outerReactive) && !containsCriticalReferences(outerReactive)
    }
  }

  def treeTypeNullWarning(tree: Tree): Unit =
    c.warning(
      c.enclosingPosition,
      "internal warning: tree type was null, " +
      "this should not happen but the signal may still work\n" +
      s"Offending tree: $tree"
    )

  def isInterpretable(tree: Tree): Boolean = {
    val staticInterpClass = c.mirror staticClass "_root_.rescala.macros.MacroAccess"

    if (tree.tpe == null) { treeTypeNullWarning(tree); false }
    else
      !(tree.tpe <:< definitions.NullTpe) &&
      !(tree.tpe <:< definitions.NothingTpe) &&
      (tree.tpe.baseClasses contains staticInterpClass)
  }

  /** detects variants to access reactives using [[rescala.macros.MacroAccess]] */
  object MacroInterpretable {
    def unapply(arg: Tree): Option[Tree] =
      arg match {
        case Apply(Select(reactive, tn), Nil) if "apply" == tn.decodedName.toString && isInterpretable(reactive) =>
          Some(reactive)
        case Select(reactive, tn) if "value" == tn.decodedName.toString && isInterpretable(reactive) =>
          Some(reactive)
        case _ => None
      }
  }

  /** find expressions annotated with @scala.annotation.unchecked */
  def calcUncheckedExpressions[A](expression: Tree): Set[c.universe.Tree] = {
    (expression collect {
      case tree @ Typed(_, _) if (tree.tpe match {
            case AnnotatedType(annotations, _) =>
              annotations exists { _.tree.tpe <:< typeOf[unchecked] }
            case _ => false
          }) =>
        def uncheckedSubExpressions(tree: Tree): List[Tree] =
          tree match {
            case Select(expr, _)    => expr :: uncheckedSubExpressions(expr)
            case Apply(expr, _)     => expr :: uncheckedSubExpressions(expr)
            case TypeApply(expr, _) => expr :: uncheckedSubExpressions(expr)
            case Typed(expr, _)     => expr :: uncheckedSubExpressions(expr)
            case Block(_, expr)     => expr :: Nil
            case _                  => Nil
          }
        uncheckedSubExpressions(tree)
    }).flatten.toSet
  }

  def checkForPotentialSideEffects[A](
      expression: Tree,
      uncheckedExpressions: Set[c.universe.Tree]
  ): Unit = {
    // collect expression annotated to be unchecked and do not issue warnings
    // for those (use the standard Scala unchecked annotation for that purpose)

    // generate warning for some common cases where called functions are
    // either unnecessary (if free of side effects) or have side effects
    def isMethodWithPotentialNonLocalSideEffects(toCheck: Tree) =
      toCheck match {
        case function @ (TypeApply(_, _) | Apply(_, _) | Select(_, _)) if !(uncheckedExpressions contains function) =>
          val arguments = toCheck match {
            case TypeApply(_, args) => args
            case Apply(_, args)     => args
            case _                  => List.empty
          }

          val noFunctionInArgs = !(arguments exists {
            case tree if (tree.tpe match {
                  case TypeRef(_, _, args) => args.nonEmpty
                  case _                   => false
                }) => true
            case _ => false
          })

          val noConstructorInFun = function exists {
            case Apply(fun, args) =>
              !(fun exists {
                case Select(_, termNames.CONSTRUCTOR) => true
                case _                                => false
              })
            case _ => false
          }

          noFunctionInArgs && noConstructorInFun
        case _ => false
      }

    def potentialSideEffectWarning(pos: Position) =
      c.warning(
        pos,
        "Statement may either be unnecessary or have side effects. " +
        "Signal expressions should have no side effects."
      )

    expression foreach {
      case Block(stats, _) =>
        stats foreach { stat =>
          if (isMethodWithPotentialNonLocalSideEffects(stat))
            potentialSideEffectWarning(stat.pos)
        }
      case tree =>
        if (
          isMethodWithPotentialNonLocalSideEffects(tree) &&
          tree.tpe != null &&
          tree.tpe =:= typeOf[Unit]
        )
          potentialSideEffectWarning(tree.pos)
    }
  }

}
