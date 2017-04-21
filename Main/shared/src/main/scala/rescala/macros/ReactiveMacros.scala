package rescala.macros

import rescala.engine.{LowPriorityTurnSource, TurnSource}
import rescala.graph.{DynamicTicket, Struct}
import rescala.reactives.{Event, Signal}
import retypecheck._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object ReactiveMacros {

  def Signal[A, S <: Struct](expression: A): Signal[A, S] = macro SignalMacro[A, S]
  def Event[A, S <: Struct](expression: A): Event[A, S] = macro EventMacro[A, S]

  def SignalMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](c: blackbox.Context)(expression: c.Expr[A]): c.Expr[Signal[A, S]] = {
    import c.universe._

    if (!c.hasErrors) {
      val (cutOutSignals, signalExpression, filteredDetections ) =  ReactiveMacro(c)(expression)

      // create SignalSynt object
      // use fully-qualified name, so no extra import is needed
      val body = q"${termNames.ROOTPKG}.rescala.reactives.Signals.dynamic[${weakTypeOf[A]}, ${weakTypeOf[S]}](..$filteredDetections)($signalExpression)"

      // assemble the SignalSynt object and the signal values that are accessed
      // by the object, but were cut out of the signal expression during the code
      // transformation
      val block = Typed(Block(cutOutSignals.reverse, body), TypeTree(weakTypeOf[Signal[A, S]]))

      c.Expr[Signal[A, S]](c.retyper untypecheck block)
    }
    else
      c.Expr[Signal[A, S]](q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("signal macro not expanded")""")
  }



  def EventMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](c: blackbox.Context)(expression: c.Expr[A]): c.Expr[Event[A, S]] = {
    import c.universe._

    if (!c.hasErrors) {
      val (cutOutSignals, signalExpression, filteredDetections ) =  ReactiveMacro(c)(expression)

      // create SignalSynt object
      // use fully-qualified name, so no extra import is needed
      val body = q"${termNames.ROOTPKG}.rescala.reactives.Events.dynamic[${weakTypeOf[A]}, ${weakTypeOf[S]}](..$filteredDetections)($signalExpression)"

      // assemble the SignalSynt object and the signal values that are accessed
      // by the object, but were cut out of the signal expression during the code
      // transformation
      val block = Typed(Block(cutOutSignals.reverse, body), TypeTree(weakTypeOf[Event[A, S]]))

      c.Expr[Event[A, S]](c.retyper untypecheck block)
    }
    else
      c.Expr[Event[A, S]](q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("event macro not expanded")""")
  }

  def ReactiveMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](c: blackbox.Context)(expression: c.Expr[A]): (List[c.universe.ValDef], c.universe.Tree, List[c.universe.Tree]) = {
    import c.universe._

    val uncheckedExpressions: Set[Tree] = calcUncheckedExpressions(c)(expression)
    checkForPotentialSideEffects(c)(expression, uncheckedExpressions)

    // all symbols that are defined within the macro expression
    val definedSymbols: Map[Symbol, DefTree] = (expression.tree collect {
      case defTree: DefTree => defTree.symbol -> defTree
    }).toMap

    // the name of the generated turn argument passed to the signal closure
    // every Signal { ... } macro instance gets expanded into a dynamic signal
    val signalMacroArgumentName = TermName(c.freshName("s$"))
    val signalSyntArgIdent = Ident(signalMacroArgumentName)
    internal setType(signalSyntArgIdent, weakTypeOf[DynamicTicket[S]])

    // the signal values that will be cut out of the Signal expression
    var cutOutSignals = List[ValDef]()
    // list of detected inner signals
    var detectedReactives = List[Tree]()

    object transformer extends Transformer {
      private def treeTypeNullWarning() =
        c.warning(c.enclosingPosition,
          "internal warning: tree type was null, " +
            "this should not happen but the signal may still work")

      private def potentialReactiveConstructionWarning(pos: Position) =
        c.warning(pos,
          "expression should not be placed inside a signal expression " +
            "since it potentially creates a new reactive every time the " +
            "signal is evaluated which can lead to unintentional behavior")

      private val signal = c.mirror staticClass "_root_.rescala.reactives.Signal"

      private val event = c.mirror staticClass "_root_.rescala.reactives.Event"

      private def isReactive(tree: Tree) =
        if (tree.tpe == null) { treeTypeNullWarning(); false }
        else
          !(tree.tpe <:< definitions.NullTpe) &&
          !(tree.tpe <:< definitions.NothingTpe) &&
          ((tree.tpe.baseClasses contains signal) || (tree.tpe.baseClasses contains event))

      private def isStatefulReactive(tree: Tree) =
        if (tree.tpe == null) { treeTypeNullWarning(); false }
        else
          !(tree.tpe <:< definitions.NullTpe) &&
          !(tree.tpe <:< definitions.NothingTpe) &&
          (tree.tpe.baseClasses contains signal)

      override def transform(tree: Tree): Tree =
        tree match {
          // replace any used TurnSource in a Signal expression with the correct turn source for the current turn
          case turnSource@q"$_.fromEngineImplicit[..$_](...$_)" if turnSource.tpe =:= weakTypeOf[TurnSource[S]] && turnSource.symbol.owner == symbolOf[LowPriorityTurnSource] =>
            q"${termNames.ROOTPKG}.rescala.engine.TurnSource.fromDynamicTicket($signalMacroArgumentName)"

          // pass the SignalSynt argument to every reactive
          // to obtain dynamic dependencies
          //
          // for example, this step transforms
          //   Signal { a() + b() }
          // to
          //   SignalSynt { s => a(s) + b(s) }
          case tree@q"$reactive.apply()"
            if isReactive(reactive) =>
            detectedReactives ::= reactive
            val reactiveApply = q"$signalSyntArgIdent.depend"
            internal setType(reactiveApply, tree.tpe)

            q"$reactiveApply(${transform(reactive)})"

          // cut signal values out of the signal expression, that could
          // potentially create a new signal object for every access
          // it is assumed that such functions are pure in the sense that they
          // will create an equivalent signal for each call with the same
          // arguments so the function value has to be calculated just once
          //
          // for example, this step transforms
          //   Signal { event.count() }
          // to
          //   Signal { s() }
          // and creates a signal value
          //   val s = event.count
          case reactive@(TypeApply(_, _) | Apply(_, _) | Select(_, _))
            if isStatefulReactive(reactive) &&
              // make sure that the expression e to be cut out
              // - refers to a term that is not a val or var
              //   or an accessor for a field
              // - is not a reactive value resulting from a function that is
              //   itself called on a reactive value
              //   (so the expression does not contained "chained" reactives)
              // - does not reference definitions that are defined within the
              //   macro expression but not within e
              //   (note that such a case can lead to unintentional behavior)
              reactive.symbol.isTerm &&
              !reactive.symbol.asTerm.isVal &&
              !reactive.symbol.asTerm.isVar &&
              !reactive.symbol.asTerm.isAccessor &&
              (reactive filter { tree =>
                val critical = tree match {
                  // check if reactive results from a function that is
                  // itself called on a reactive value
                  case q"$chainedReactive.apply()" =>
                    isStatefulReactive(chainedReactive)

                  // check reference definitions that are defined within the
                  // macro expression but not within the reactive
                  case tree: SymTree =>
                    definedSymbols get tree.symbol match {
                      case Some(defTree) => !(reactive exists { _ == defTree })
                      case _ => false
                    }

                  // "uncritical" reactive that can be cut out
                  case _ => false
                }

                if (critical && !(uncheckedExpressions contains reactive)) {
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
                      case tpe @ TypeRef(_, _, _) => tpe
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

                critical
              }).isEmpty =>

            // create the signal definition to be cut out of the
            // macro expression and its substitution variable
            val signalName = TermName(c.freshName("s$"))

            val signalDef = ValDef(Modifiers(), signalName, TypeTree(), reactive)
            cutOutSignals ::= signalDef

            val ident = Ident(signalName)
            internal setType(ident, reactive.tpe)
            ident

          case _ =>
            super.transform(tree)
        }
    }

    val innerTree = transformer transform expression.tree

    // SignalSynt argument function
    val signalExpression = q"{$signalMacroArgumentName: ${ weakTypeOf[DynamicTicket[S]] } => $innerTree }"

    // upper bound parameters, only use static outside declarations
    // note that this potentially misses many dependencies
    // these will be detected dynamically, but that may cause multiple evaluations when creating a signal
    val filteredDetections = detectedReactives.filter(tree =>
      !definedSymbols.contains(tree.symbol) &&
        tree.symbol.isTerm &&
        (tree.symbol.asTerm.isVal ||
          tree.symbol.asTerm.isVar))

    (cutOutSignals, signalExpression, filteredDetections)
  }




  def calcUncheckedExpressions[A: c.WeakTypeTag](c: blackbox.Context)(expression: c.Expr[A]): Set[c.universe.Tree] = {
    import c.universe._
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

  def checkForPotentialSideEffects[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
    (c: blackbox.Context)
    (expression: c.Expr[A], uncheckedExpressions: Set[c.universe.Tree]): Unit = {

    import c.universe._
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
          tree.tpe =:= typeOf[Unit])
          potentialSideEffectWarning(tree.pos)
    }
  }

}
