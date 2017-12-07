package rescala.macros

import rescala.core.{CreationTicket, DynamicTicket, LowPriorityCreationImplicits, Struct}
import rescala.reactives.{Event, Signal}
import retypecheck._

import scala.reflect.macros.blackbox

class ReactiveMacros(val c: blackbox.Context) {
  import c.universe._

  def SignalMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](expression: c.Expr[A])(ticket: c.Tree): c.Expr[Signal[A, S]] = {

    if (!c.hasErrors) {
      val (cutOutSignals, signalExpression, filteredDetections, _) = ReactiveMacro(expression)

      // create SignalSynt object
      // use fully-qualified name, so no extra import is needed
      val body = q"${termNames.ROOTPKG}.rescala.reactives.Signals.dynamic[${weakTypeOf[A]}, ${weakTypeOf[S]}](..$filteredDetections)($signalExpression)($ticket)"

      // assemble the SignalSynt object and the signal values that are accessed
      // by the object, but were cut out of the signal expression during the code
      // transformation
      val block = Typed(Block(cutOutSignals.reverse, body), TypeTree(weakTypeOf[Signal[A, S]]))

      c.Expr[Signal[A, S]](c.retyper untypecheck block)
    }
    else
      c.Expr[Signal[A, S]](q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("signal macro not expanded")""")
  }


  def EventMapMacro[T: c.WeakTypeTag, A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](expression: c.Expr[T => A])(ticket: c.Tree): c.Expr[Event[A, S]] = {

    if (!c.hasErrors) {

      val mapFunctionArgumentTermName = TermName(c.freshName("eventValue$"))
      val mapFunctionArgumentIdent = Ident(mapFunctionArgumentTermName)
      internal setType(mapFunctionArgumentIdent, weakTypeOf[Event[T, S]])

      val constructedExpression = q"""$mapFunctionArgumentIdent.apply().map($expression)"""
      val (cutOutSignals, transformedExpression, filteredDetections, detections) = ReactiveMacro(c.Expr[Option[A]](constructedExpression))

      val body = if (detections.size > 1) {
        val extendedDetections = mapFunctionArgumentIdent :: filteredDetections

        // create SignalSynt object
        // use fully-qualified name, so no extra import is needed
        q"""{
          val $mapFunctionArgumentTermName = ${c.prefix}
          ${termNames.ROOTPKG}.rescala.reactives.Events.dynamic[${weakTypeOf[A]}, ${weakTypeOf[S]}](..$extendedDetections){$transformedExpression}($ticket)
        }"""
      } else {
        q"""${c.prefix}.staticMap($expression)($ticket)"""
      }

      // assemble the SignalSynt object and the signal values that are accessed
      // by the object, but were cut out of the signal expression during the code
      // transformation
      val block = Typed(Block(cutOutSignals.reverse, body), TypeTree(weakTypeOf[Event[A, S]]))

      c.Expr[Event[A, S]](c.retyper untypecheck block)
    }
    else
      c.Expr[Event[A, S]](q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("event macro not expanded")""")
  }

  def EventMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](expression: c.Expr[A])(ticket: c.Tree): c.Expr[Event[A, S]] = {

    if (!c.hasErrors) {
      val (cutOutSignals, signalExpression, filteredDetections, _) = ReactiveMacro(expression)

      // create SignalSynt object
      // use fully-qualified name, so no extra import is needed
      val body = q"${termNames.ROOTPKG}.rescala.reactives.Events.dynamic[${weakTypeOf[A]}, ${weakTypeOf[S]}](..$filteredDetections)($signalExpression)($ticket)"

      // assemble the SignalSynt object and the signal values that are accessed
      // by the object, but were cut out of the signal expression during the code
      // transformation
      val block = Typed(Block(cutOutSignals.reverse, body), TypeTree(weakTypeOf[Event[A, S]]))

      c.Expr[Event[A, S]](c.retyper untypecheck block)
    }
    else
      c.Expr[Event[A, S]](q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("event macro not expanded")""")
  }



  def ReactiveMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](expression: c.Expr[A]): (List[c.universe.ValDef], c.universe.Tree, List[c.universe.Tree], List[c.universe.Tree]) = {


//    def treeTypeNullWarning(): Unit =
//            c.warning(c.enclosingPosition,
//              "internal warning: tree type was null, " +
//                "this should not happen but the signal may still work")



    val uncheckedExpressions: Set[Tree] = calcUncheckedExpressions(expression)
    checkForPotentialSideEffects(expression, uncheckedExpressions)

    // all symbols that are defined within the macro expression
    val symbolsDefinedInsideMacroExpression: Map[Symbol, DefTree] = (expression.tree collect {
      case defTree: DefTree => defTree.symbol -> defTree
    }).toMap

    // the name of the generated turn argument passed to the signal closure
    // every Signal { ... } macro instance gets expanded into a dynamic signal
    val signalTicketTermName = TermName(c.freshName("ticket$"))
    val signalTicketIdent = Ident(signalTicketTermName)
    internal setType(signalTicketIdent, weakTypeOf[DynamicTicket[S]])

    // the signal values that will be cut out of the Signal expression
    var cutOutSignals = List[ValDef]()
    // list of detected inner signals
    var detectedReactives = List[Tree]()

    object transformer extends Transformer {

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

      private def isReactiveThatCanBeCutOut(reactive: c.universe.Tree): Boolean = {
        isReactive(reactive) &&
          reactive.symbol.isTerm &&
          !reactive.symbol.asTerm.isVal &&
          !reactive.symbol.asTerm.isVar &&
          !reactive.symbol.asTerm.isAccessor &&
          !containsCriticalReferences(reactive)
      }

      override def transform(tree: Tree): Tree =
        tree match {
          // replace any used CreationTicket in a Signal expression with the correct turn source for the current turn
          case turnSource@q"$_.fromEngineImplicit[..$_](...$_)" if turnSource.tpe =:= weakTypeOf[CreationTicket[S]] && turnSource.symbol.owner == symbolOf[LowPriorityCreationImplicits] =>
            q"${termNames.ROOTPKG}.rescala.core.CreationTicket.fromTicketDImplicit($signalTicketTermName, ${termNames.ROOTPKG}.rescala.core.REName.create)"

          // Access every reactive through the ticket argument
          // to obtain dynamic dependencies
          //
          // for example, this step transforms
          //   Signal { reactive_1() + reactive_2() }
          // to
          //   Signals.dynamic { s => s.depend(reactive_1) + s.depend(reactive_2) }
          case tree@REApply(reactive) =>
            detectedReactives ::= reactive
            val reactiveApply = q"$signalTicketIdent.depend"
            internal.setType(reactiveApply, tree.tpe)
            q"$reactiveApply(${transform(reactive)})"

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
          case reactive@(TypeApply(_, _) | Apply(_, _) | Select(_, _)) if isReactiveThatCanBeCutOut(reactive) =>

            // create the signal definition to be cut out of the
            // macro expression and its substitution variable
            val cutOutReactiveTermName = TermName(c.freshName("reactive$"))

            val signalDef = q"val $cutOutReactiveTermName = $reactive"
            cutOutSignals ::= signalDef

            val ident = Ident(cutOutReactiveTermName)
            internal setType(ident, reactive.tpe)
            ident

          case _ =>
            super.transform(tree)
        }
    }

    val innerTree = transformer transform expression.tree

    // SignalSynt argument function
    val signalExpression = q"{$signalTicketTermName: ${ weakTypeOf[DynamicTicket[S]] } => $innerTree }"

    // upper bound parameters, only use static outside declarations
    // note that this potentially misses many dependencies
    // these will be detected dynamically, but that may cause multiple evaluations when creating a signal
    val filteredDetections = detectedReactives.filter(tree =>
      tree.forAll{ t => !symbolsDefinedInsideMacroExpression.contains(t.symbol)} &&
        tree.symbol.isTerm &&
        (tree.symbol.asTerm.isVal ||
          tree.symbol.asTerm.isVar))

    (cutOutSignals, signalExpression, filteredDetections, detectedReactives)
  }

  def isReactive(tree: Tree): Boolean = {
    val staticSignalClass = c.mirror staticClass "_root_.rescala.reactives.Signal"
    val staticEventClass = c.mirror staticClass "_root_.rescala.reactives.Event"

    if (tree.tpe == null) {/*treeTypeNullWarning();*/ false}
    else
      !(tree.tpe <:< definitions.NullTpe) &&
        !(tree.tpe <:< definitions.NothingTpe) &&
        ((tree.tpe.baseClasses contains staticSignalClass) || (tree.tpe.baseClasses contains staticEventClass))
  }

  object REApply {
    def unapply(arg: Tree): Option[Tree] = arg match {
      case q"$reactive.apply()" if isReactive(reactive) => Some(reactive)
      case q"$reactive.!" if isReactive(reactive) => Some(reactive)
      case q"$reactive.unary_!" if isReactive(reactive)  => Some(reactive)
      case q"$reactive.value" if isReactive(reactive)  => Some(reactive)
      case _ => None
    }
  }

  /** find expressions annotated with @scala.annotation.unchecked  */
  def calcUncheckedExpressions[A: c.WeakTypeTag](expression: c.Expr[A]): Set[c.universe.Tree] = {
    (expression.tree collect {
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
    (expression: c.Expr[A], uncheckedExpressions: Set[c.universe.Tree]): Unit = {
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

    expression.tree foreach {
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
