package rescala.macros

import rescala.core.{CreationTicket, DynamicTicket, LowPriorityCreationImplicits, StaticTicket, Struct}
import retypecheck._

import scala.reflect.macros.blackbox

class ReactiveMacros(val c: blackbox.Context) {

  import c.universe._

  def SignalMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (expression: c.Expr[A])(ticket: c.Tree): c.Tree = ReactiveExpression(TermName("Signals"), expression, ticket)

  def EventMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (expression: c.Expr[A])(ticket: c.Tree): c.Tree = ReactiveExpression(TermName("Events"), expression, ticket)

  def ReactiveExpression[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (signalsOrEvents: TermName, expression: c.Expr[A], ticket: c.Tree): c.Tree = {

    if (c.hasErrors)
      return q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

    val mp = ReactiveMacro(expression)
    val creationMethod = TermName(if (mp.hasOnlyStaticAccess()) "static" else "dynamic")
    val body =
      q"""${termNames.ROOTPKG}.rescala.reactives.$signalsOrEvents.$creationMethod[${weakTypeOf[A]}, ${weakTypeOf[S]}](
         ..${mp.detectedStaticReactives ::: mp.cutOutReactiveTerms}
         ){${mp.userComputation}}($ticket)"""
    mp.makeBlock(body)
  }


  def EventMapMacro[T: c.WeakTypeTag, A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag]
  (expression: c.Expr[T => A])(ticket: c.Tree): c.Tree = {
    if (c.hasErrors)
      return q"""throw new ${termNames.ROOTPKG}.scala.NotImplementedError("macro not expanded because of other compilation errors")"""

    val mp = ReactiveMacro(expression)

    val body = if (mp.detectedReactives.isEmpty) {
      q"""${c.prefix}.staticMap($expression)($ticket)"""
    }
    else {

      val mapFunctionArgumentTermName = TermName(c.freshName("eventValue$"))
      val mapFunctionArgumentIdent = Ident(mapFunctionArgumentTermName)
      //internal setType(mapFunctionArgumentIdent, weakTypeOf[Event[T, S]])

      val extendedDetections = mapFunctionArgumentIdent :: mp.detectedStaticReactives ::: mp.cutOutReactiveTerms

      val valueAccessMethod = TermName(if (mp.hasOnlyStaticAccess()) "staticDepend" else "depend")
      val mp2 = mp.copy(innerTree = q"""${mp.ticketTermName}.$valueAccessMethod($mapFunctionArgumentIdent).map(${mp.innerTree})""")


      val creationMethod = TermName(if (mp.hasOnlyStaticAccess()) "static" else "dynamic")
      q"""{
        val $mapFunctionArgumentTermName = ${c.prefix}
        ${termNames.ROOTPKG}.rescala.reactives.Events.$creationMethod[${weakTypeOf[A]}, ${weakTypeOf[S]}](..$extendedDetections){${mp2.userComputation}}($ticket)
      }"""
    }

    mp.makeBlock(body)
  }



  case class MacroPieces(
    innerTree: Tree,
    ticketTermName : TermName,
    cutOutReactiveVals : List[ValDef],
    cutOutReactiveTerms : List[Tree],
    detectedStaticReactives: List[Tree],
    detectedReactives : List[Tree] ) {
    def userComputation[S <: Struct : c.WeakTypeTag]: Tree = {
      val ticketType = if (hasOnlyStaticAccess()) weakTypeOf[StaticTicket[S]] else weakTypeOf[DynamicTicket[S]]
      q"{$ticketTermName: $ticketType => $innerTree }"
    }

    def makeBlock[S <: Struct : c.WeakTypeTag, A: c.WeakTypeTag](body: c.universe.Tree): c.Tree = {
      val block: c.universe.Tree = Block(cutOutReactiveVals.reverse, body)
      ReTyper(c).untypecheck(block)
    }

    def hasOnlyStaticAccess(): Boolean = detectedStaticReactives.size == detectedReactives.size

  }

  object IsCutOut

  def ReactiveMacro[A: c.WeakTypeTag, S <: Struct : c.WeakTypeTag](expression: c.Expr[A]): MacroPieces = {
    val weAnalysis = new WholeExpressionAnalysis(expression.tree)
    // the name of the generated turn argument passed to the signal closure
    // every Signal { ... } macro instance gets expanded into a dynamic signal
    val ticketTermName: TermName = TermName(c.freshName("ticket$"))
    val ticketIdent: Ident = Ident(ticketTermName)
    //internal setType(ticketIdent, weakTypeOf[DynamicTicket[S]])

    // the signal values that will be cut out of the Signal expression
    var cutOutReactivesVals = List[ValDef]()
    var cutOutReactiveTerms = List[Tree]()
    // list of detected inner signals
    var detectedReactives = List[Tree]()
    var detectedStaticReactives = List[Tree]()

    object transformer extends Transformer {

      override def transform(tree: Tree): Tree =
        tree match {
          // replace any used CreationTicket in a Signal expression with the correct turn source for the current turn
          //q"$_.fromEngineImplicit[..$_](...$_)"
          case turnSource@Apply(TypeApply(Select(_, TermName("fromEngineImplicit")), _), _)
            if turnSource.tpe =:= weakTypeOf[CreationTicket[S]] && turnSource.symbol.owner == symbolOf[LowPriorityCreationImplicits] =>
            println(turnSource)
            q"${termNames.ROOTPKG}.rescala.core.CreationTicket(${termNames.ROOTPKG}.scala.Left($ticketIdent.creation))(${termNames.ROOTPKG}.rescala.core.REName.create)"

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
            detectedReactives ::= reactive
            val reactiveApply =
              if (weAnalysis.isStaticDependency(reactive)) {
                detectedStaticReactives ::= reactive
                q"$ticketIdent.staticDepend"
              }
              else q"$ticketIdent.depend"
            internal.setType(reactiveApply, tree.tpe)
            q"$reactiveApply($reactive)"

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
          case reactive@(TypeApply(_, _) | Apply(_, _) | Select(_, _)) if weAnalysis.isReactiveThatCanBeCutOut(reactive) =>

            // create the signal definition to be cut out of the
            // macro expression and its substitution variable
            val cutOutReactiveTermName = TermName(c.freshName("reactive$"))

            val signalDef = q"val $cutOutReactiveTermName = $reactive"
            cutOutReactiveTerms ::= Ident(cutOutReactiveTermName)
            cutOutReactivesVals ::= signalDef

            val ident = Ident(cutOutReactiveTermName)
            internal updateAttachment (ident, IsCutOut)
            internal setType(ident, reactive.tpe)
            ident

          case _ =>
            super.transform(tree)
        }
    }

    val innerTree = transformer transform expression.tree

    MacroPieces(innerTree, ticketTermName, cutOutReactivesVals, cutOutReactiveTerms, detectedStaticReactives, detectedReactives)
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
      isReactive(reactive) &&
        reactive.symbol.isTerm &&
        !reactive.symbol.asTerm.isVal &&
        !reactive.symbol.asTerm.isVar &&
        !reactive.symbol.asTerm.isAccessor &&
        !containsCriticalReferences(reactive)
    }

  }


  def treeTypeNullWarning(): Unit =
    c.warning(c.enclosingPosition,
      "internal warning: tree type was null, " +
        "this should not happen but the signal may still work")

  def isReactive(tree: Tree): Boolean = {
    val staticSignalClass = c.mirror staticClass "_root_.rescala.reactives.Signal"
    val staticEventClass = c.mirror staticClass "_root_.rescala.reactives.Event"

    if (tree.tpe == null) {treeTypeNullWarning(); false}
    else
      !(tree.tpe <:< definitions.NullTpe) &&
        !(tree.tpe <:< definitions.NothingTpe) &&
        ((tree.tpe.baseClasses contains staticSignalClass) || (tree.tpe.baseClasses contains staticEventClass))
  }

  object REApply {
    def unapply(arg: Tree): Option[Tree] = arg match {
      case Apply(Select(reactive, TermName(tn)), Nil)
        if List("apply").contains(tn) && isReactive(reactive) =>
        Some(reactive)
      case Select(reactive, tn)
        if List("!", "unary_!", "value").contains(tn.decodedName.toString) && isReactive(reactive) =>
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
