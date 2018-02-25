package rescala.macros

import rescala.core.{CreationTicket, DynamicTicket, LowPriorityCreationImplicits, StaticTicket, Struct}
import retypecheck._

import scala.reflect.macros.blackbox

object MacroTags {
  type Staticism
  type Static <: Staticism
  type Dynamic <: Staticism
}

class ReactiveMacros(val c: blackbox.Context) {

  import c.universe._

  def ReactiveExpression[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag, IsStatic <: MacroTags.Staticism : c.WeakTypeTag, ReactiveType : c.WeakTypeTag]
  (expression: c.Expr[A])(ticket: c.Tree): c.Tree = {

    if (c.hasErrors)
      return q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

    val ticketTermName: TermName = TermName(c.freshName("ticket$"))
    val ticketIdent: Ident = Ident(ticketTermName)

    val forceStatic = !(weakTypeOf[IsStatic] <:< weakTypeOf[MacroTags.Dynamic])
    val weAnalysis = new WholeExpressionAnalysis(expression.tree)
    val cutOut = new CutOutTransformer(weAnalysis)
    val cutOutTree = cutOut.transform(expression.tree)
    val detections = new RewriteTransformer(weAnalysis, ticketIdent, forceStatic)
    val rewrittenTree = detections transform cutOutTree

    makeExpression[S, A](
      weakTypeOf[ReactiveType].typeSymbol.asClass.module,
      ticket,
      detections.detectedStaticReactives,
      detections.detectedDynamicReactives.isEmpty,
      ticketTermName,
      rewrittenTree,
      cutOut.cutOutReactivesVals.reverse)
  }


  def EventMapMacro[T: c.WeakTypeTag, A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (expression: c.Expr[T => A])(ticket: c.Tree): c.Tree = {
    if (c.hasErrors)
      return q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

    val ticketTermName: TermName = TermName(c.freshName("ticket$"))
    val ticketIdent: Ident = Ident(ticketTermName)

    val weAnalysis = new WholeExpressionAnalysis(expression.tree)
    val cutOut = new CutOutTransformer(weAnalysis)
    val cutOutTree = cutOut.transform(expression.tree)
    val detections = new RewriteTransformer[S](weAnalysis, ticketIdent, requireStatic = true)
    val rewrittenTree = detections transform cutOutTree


    val mapFunctionArgumentTermName = TermName(c.freshName("eventValue$"))
    val mapFunctionArgumentIdent = Ident(mapFunctionArgumentTermName)

    val extendedDetections = mapFunctionArgumentIdent :: detections.detectedStaticReactives ::: cutOut.cutOutReactiveIdentifiers

    val valueAccessMethod = TermName(if (detections.detectedDynamicReactives.isEmpty) "dependStatic" else "depend")

    val mapTree = q"""$ticketTermName.$valueAccessMethod($mapFunctionArgumentIdent).map($rewrittenTree)"""

    makeExpression[S, A](
      weakTypeOf[rescala.reactives.Events.type].termSymbol,
      ticket,
      extendedDetections,
      detections.detectedDynamicReactives.isEmpty,
      ticketTermName,
      mapTree,
      (q"val $mapFunctionArgumentTermName = ${c.prefix}" : ValDef) :: cutOut.cutOutReactivesVals.reverse
    )

  }

  def EventFoldMacro[T: c.WeakTypeTag, A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (init: c.Expr[A])(op: c.Expr[(A, T) => A])(ticket: c.Expr[rescala.core.CreationTicket[S]], serializable: c.Expr[rescala.core.ReSerializable[A]]): c.Tree = {
    if (c.hasErrors)
      return q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

    val innerTicketTermName: TermName = TermName(c.freshName("ticket$"))
    val innerTicketIdent: Ident = Ident(innerTicketTermName)

    val weAnalysis = new WholeExpressionAnalysis(op.tree)
    val cutOut = new CutOutTransformer(weAnalysis)
    val cutOutTree = cutOut.transform(op.tree)
    val detections = new RewriteTransformer[S](weAnalysis, innerTicketIdent, requireStatic = true)
    val rewrittenTree = detections transform cutOutTree


    val mapFunctionArgumentTermName = TermName(c.freshName("eventValue$"))
    val mapFunctionArgumentIdent = Ident(mapFunctionArgumentTermName)

    val extendedDetections = mapFunctionArgumentIdent :: detections.detectedStaticReactives ::: cutOut.cutOutReactiveIdentifiers

    val eventsSymbol = weakTypeOf[rescala.reactives.Events.type].termSymbol


    // def fold[T: ReSerializable, S <: Struct](dependencies: Set[ReSource[S]], init: T)(expr: (StaticTicket[S], () => T) => T)(implicit ticket: CreationTicket[S]): Signal[T, S] = {
    val valDefs = (q"val $mapFunctionArgumentTermName = ${c.prefix}": ValDef) :: cutOut.cutOutReactivesVals.reverse
    val accumulatorIdent: Ident = Ident( TermName(c.freshName("accumulator$")))
    val pulseIdent: Ident = Ident( TermName(c.freshName("pulse")))
    val ticketType = weakTypeOf[StaticTicket[S]]
    val body =
      q"""$eventsSymbol.fold[${weakTypeOf[A]}, ${weakTypeOf[S]}](Set(..$extendedDetections), $init)(
          ($innerTicketIdent: $ticketType, $accumulatorIdent: ${weakTypeOf[T]}) => {
            val $pulseIdent = $innerTicketIdent.dependStatic($mapFunctionArgumentIdent).get
            $rewrittenTree($accumulatorIdent(), $pulseIdent)
          })($serializable, $ticket)"""
    val block: c.universe.Tree = Block(valDefs, body)
    ReTyper(c).untypecheck(block)

  }

  private def makeExpression[S <: Struct : c.WeakTypeTag, A: c.WeakTypeTag](
    signalsOrEvents: c.universe.Symbol,
    outerTicket: c.Tree,
    dependencies: Seq[Tree],
    isStatic: Boolean,
    innerTicket: TermName,
    innerTree: Tree,
    valDefs: List[ValDef],
  ): Tree = {
    val creationMethod = TermName(if (isStatic) "static" else "dynamic")
    val ticketType = if (isStatic) weakTypeOf[StaticTicket[S]] else weakTypeOf[DynamicTicket[S]]
    val computation = q"{$innerTicket: $ticketType => $innerTree }"
    val body = q"""$signalsOrEvents.$creationMethod[${weakTypeOf[A]}, ${weakTypeOf[S]}](
         ..$dependencies
         ){$computation}($outerTicket)"""
    val block: c.universe.Tree = Block(valDefs, body)
    ReTyper(c).untypecheck(block)
  }

  object IsCutOut

  class RewriteTransformer[S <: Struct : WeakTypeTag](weAnalysis: WholeExpressionAnalysis, ticketIdent: Ident, requireStatic: Boolean) extends Transformer {

    // list of detected inner signals
    var detectedDynamicReactives = List[Tree]()
    var detectedStaticReactives = List[Tree]()

    override def transform(tree: Tree): Tree =
      tree match {
        // replace any used CreationTicket in a Signal expression with the correct turn source for the current turn
        //q"$_.fromEngineImplicit[..$_](...$_)"
        case turnSource@Apply(TypeApply(Select(_, TermName("fromEngineImplicit")), _), _)
          if turnSource.tpe =:= weakTypeOf[CreationTicket[S]] && turnSource.symbol.owner == symbolOf[LowPriorityCreationImplicits] =>
          q"""${termNames.ROOTPKG}.rescala.core.CreationTicket(
                  ${termNames.ROOTPKG}.scala.Left($ticketIdent.creation))(
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
        case tree@REApply(untransformedReactive) =>
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
          q"$reactiveApply($reactive)"
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
        case reactive@(TypeApply(_, _) | Apply(_, _) | Select(_, _) | Block(_, _) | Typed(_, _))
          if weAnalysis.isReactiveThatCanBeCutOut(reactive) =>

          // create the signal definition to be cut out of the
          // macro expression and its substitution variable
          val cutOutReactiveTermName = TermName(c.freshName("reactive$"))
          val ident = Ident(cutOutReactiveTermName)
          val signalDef: ValDef = q"val $cutOutReactiveTermName = $reactive"

          cutOutReactivesVals ::= signalDef
          cutOutReactiveIdentifiers ::= ident

          internal updateAttachment (ident, IsCutOut)
          internal setType(ident, reactive.tpe)
          ident

        case _ =>
          super.transform(tree)
      }
  }


  class WholeExpressionAnalysis(expression: Tree) {
    val uncheckedExpressions: Set[Tree] = calcUncheckedExpressions(expression)

    // all symbols that are defined within the macro expression
    val symbolsDefinedInsideMacroExpression: Map[Symbol, DefTree] = (expression collect {
      case defTree: DefTree => defTree.symbol -> defTree
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
    def containsCriticalReferences(reactive: c.universe.Tree): Boolean = reactive.filter { tree =>
      val critical = tree match {
        // check if reactive results from a function that is
        // itself called on a reactive value
        case REApply(_) => true
        // check reference definitions that are defined within the
        // macro expression but not within the reactive
        case tree: SymTree =>
          symbolsDefinedInsideMacroExpression get tree.symbol match {
            case Some(defTree) => !(reactive exists {_ == defTree})
            case _ => false
          }

        // "uncritical" reactive that can be cut out
        case _ => false
      }

      if (critical) checkAndWarnReactiveConstructions(reactive, uncheckedExpressions)

      critical
    }.nonEmpty

    def isReactiveThatCanBeCutOut(reactive: c.universe.Tree): Boolean = {
      isInterpretable(reactive) &&
        (reactive match {
          case Block(_, _) =>
            true
          case Typed(expr, _) =>
            isReactiveThatCanBeCutOut(expr)
          case _ => reactive.symbol.isTerm &&
            !reactive.symbol.asTerm.isVal &&
            !reactive.symbol.asTerm.isVar &&
            !reactive.symbol.asTerm.isAccessor
        }) &&
        !containsCriticalReferences(reactive)
    }

  }


  def treeTypeNullWarning(): Unit =
    c.warning(c.enclosingPosition,
      "internal warning: tree type was null, " +
        "this should not happen but the signal may still work")

  def isInterpretable(tree: Tree): Boolean = {
    val staticInterpClass = c.mirror staticClass "_root_.rescala.core.Interp"

    if (tree.tpe == null) {treeTypeNullWarning(); false}
    else
      !(tree.tpe <:< definitions.NullTpe) &&
        !(tree.tpe <:< definitions.NothingTpe) &&
        ((tree.tpe.baseClasses contains staticInterpClass))
  }

  /** detects variants to access reactives using [[Interp]] */
  object REApply {
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



  def checkAndWarnReactiveConstructions(reactive: c.universe.Tree, uncheckedExpressions: Set[c.universe.Tree]) = {

    def potentialReactiveConstructionWarning(pos: Position): Unit =
      c.warning(pos,
        "expression should not be placed inside a signal expression " +
          "since it potentially creates a new reactive every time the " +
          "signal is evaluated which can lead to unintentional behavior")

    if (!uncheckedExpressions.contains(reactive)) {
      def methodObjectType(method: Tree) = {
        def methodObjectType(tree: Tree): Type =
          if (tree.symbol != method.symbol)
            tree.tpe
          else if (tree.children.nonEmpty)
            methodObjectType(tree.children.head)
          else
            NoType

        methodObjectType(method) match {
          // if we can access the type arguments of the type directly,
          // return it
          case tpe@TypeRef(_, _, _) => tpe
          // otherwise, find the type in the term symbol's type signature
          // whose type arguments can be accessed
          case tpe =>
            tpe.termSymbol.typeSignature find {
              case TypeRef(_, _, _) => true
              case _ => false
            } match {
              case Some(tpe) => tpe
              case _ => tpe
            }
        }
      }

      // issue no warning if the reactive is retrieved from a container
      // determined by the generic type parameter
      methodObjectType(reactive) match {
        case TypeRef(_, _, args) =>
          if (!(args contains reactive.tpe))
            potentialReactiveConstructionWarning(reactive.pos)
        case _ =>
          potentialReactiveConstructionWarning(reactive.pos)
      }
    }
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
