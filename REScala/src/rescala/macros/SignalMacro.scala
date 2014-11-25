package rescala.macros

import rescala.propagation.Stateful
import rescala.propagation.turns.Turn
import rescala.signals._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object SignalMacro {

  def SignalM[A](expression: A): Signal[A] = macro SignalMacro[A]

  def SignalMacro[A: c.WeakTypeTag](c: blackbox.Context)(expression: c.Expr[A]): c.Expr[Signal[A]] = {
    import c.universe._

    // all symbols that are defined within the macro expression
    val definedSymbols = (expression.tree collect {
      case defTree: DefTree => defTree.symbol -> defTree
    }).toMap

    // find inner macros that are not expanded
    // (note: should inner macros not be expanded first by the compiler?)
    // (remark: they are, at least for scala 2.11.2 `SignalM { SignalM { 1 } }` does not trigger the code below)
    // we need to take special care for nested signals
    val nestedUnexpandedMacros = (expression.tree collect {
      case tree if tree.symbol != null && tree.symbol.isMacro =>
        val innerMacro = tree match {
          case Apply(TypeApply(Select(makro, _), _), _) => makro
          case TypeApply(Select(makro, _), _) => makro
          case Select(makro, _) => makro
          case _ => null
        }

        if (innerMacro != null && innerMacro.tpe =:= typeOf[this.type])
          tree :: (tree filter { _ => true })
        else
          List.empty
    }).flatten.toSet

    // collect expression annotated to be unchecked and do not issue warnings
    // for those (use the standard Scala unchecked annotation for that purpose)
    val uncheckedExpressions = (expression.tree collect {
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

    // the argument that is used by the SignalSynt class to assemble dynamic
    // dependencies
    // every Signal { ... } macro instance gets expanded into a SignalSynt
    val signalSyntArgName = TermName(c.freshName("s$"))
    val signalSyntArgIdent = Ident(signalSyntArgName)
    internal setType(signalSyntArgIdent, weakTypeOf[Turn])

    // the signal values that will be cut out of the Signal expression
    var cutOutSignals = List[ValDef]()
    // list of detected inner signals
    var detectedSignals = List[Tree]()

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

      private def isStateful(tree: Tree) =
        if (tree.tpe == null) { treeTypeNullWarning(); false }
        else tree.tpe <:< typeOf[Stateful[_]]

      override def transform(tree: Tree) =
        tree match {
          // pass the SignalSynt argument to every reactive
          // to obtain dynamic dependencies
          //
          // for example, this step transforms
          //   Signal { a() + b() }
          // to
          //   SignalSynt { s => a(s) + b(s) }
          case tree@q"$reactive.apply()"
            if isStateful(reactive)
              && !(nestedUnexpandedMacros contains tree) =>
            detectedSignals ::= reactive
            val reactiveApply = Select(reactive, TermName("apply"))
            internal setType(reactiveApply, tree.tpe)
            Apply(super.transform(reactiveApply), List(signalSyntArgIdent))

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
            if isStateful(reactive) &&
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
                val citical = tree match {
                  // check if reactive results from a function that is
                  // itself called on a reactive value
                  case tree@Apply(Select(chainedReactive, apply), List()) =>
                    isStateful(chainedReactive) &&
                      apply.decodedName.toString == "apply" &&
                      !(nestedUnexpandedMacros contains tree)

                  // check reference definitions that are defined within the
                  // macro expression but not within the reactive
                  case tree: SymTree =>
                    definedSymbols get tree.symbol match {
                      case Some(defTree) => !(reactive exists { _ == defTree })
                      case _ => false
                    }

                  // "uncitical" reactive that can be cut out
                  case _ => false
                }

                if (citical && !(uncheckedExpressions contains reactive)) {
                  def methodObjectType(method: Tree) = {
                    def methodObjectType(tree: Tree): Type =
                      if (tree.symbol != method.symbol)
                        tree.tpe
                      else if (tree.children.nonEmpty)
                        methodObjectType(tree.children.head)
                      else
                        NoType

                    methodObjectType(method)
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

                citical
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
    val signalExpression = q"{$signalSyntArgName: ${ weakTypeOf[Turn] } => $innerTree }"

    // upper bound parameters, only use static outside declarations
    // note that this potentially misses many dependencies
    // these will be detected dynamically, but that may cause multiple evaluations when creating a signal
    val filteredDetections = detectedSignals.filter(tree =>
      !definedSymbols.contains(tree.symbol) &&
        tree.symbol.isTerm &&
        (tree.symbol.asTerm.isVal ||
          tree.symbol.asTerm.isVar))

    // create SignalSynt object
    // use fully-qualified name, so no extra import is needed
    val body = q"rescala.signals.Signals.dynamic[${ weakTypeOf[A] }](..$filteredDetections)($signalExpression)"

    // assemble the SignalSynt object and the signal values that are accessed
    // by the object, but were cut out of the signal expression during the code
    // transformation
    val block =
      Typed(Block(cutOutSignals.reverse, body), TypeTree(weakTypeOf[Signal[A]]))


    c.Expr[Signal[A]](c untypecheck block)
  }
}
