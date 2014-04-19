package `macro`

import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.Context

import react.Signal
import react.SignalSynt
import react.Var

object SignalMacro {
  
  def SignalM[A](expression: A): Signal[A] = macro SignalMacro[A]
  
  def SignalMacro[A: c.WeakTypeTag](c: Context)(expression: c.Expr[A]):
      c.Expr[Signal[A]] = {
    import c.universe._
    
    
//    val out = new java.io.FileWriter(
//        "/home/pascal/Desktop/debugfile.txt", true)
    
//    out.append(showRaw(expression.tree) + "\n\n\n\n")
//    out.append(showRaw(reify { { val a = 0; a } }) + "\n\n\n\n")
    
//    extracting sub trees with type Reactive[_]
//    val extractedReactives = expression.tree filter {
//      t => t.tpe <:< typeOf[Reactive]
//    }
    
    
    // all symbols that are defined within the macro expression
    val definedSymbols = (expression.tree collect {
      case defTree: DefTree => defTree.symbol -> defTree
    }).toMap
    
    // generate warning for some common cases where called functions are 
    // either unnecessary (if free of side effects) or have side effects
    expression.tree foreach {
      case Block(stats, _) =>
        stats foreach {
          case stat @ Apply(_, _) 
            if (stat exists {
              case stat @ Apply(fun, _) =>
                !(fun exists {
                  case t @ Select(_, nme.CONSTRUCTOR) => true
                  case tree: SymTree => definedSymbols contains tree.symbol
                  case _ => false
                })
              case _ => false
            }) =>
            c.warning(stat.pos,
                "Statement is either unnecessary or has side effects. " +
                "Signal expressions should have no side effects.")
          case _ =>
        }
      case _ =>
    }
    
    // the argument that is used by the SignalSynt class to assemble dynamic
    // dependencies
    // every Signal { ... } macro instance gets expanded into a SignalSynt
    val signalSyntArgName = newTermName(c.fresh("s$"))
    val signalSyntArgIdent = Ident(signalSyntArgName)
    signalSyntArgIdent.tpe = weakTypeOf[SignalSynt[A]]
    
    // the signal values that will be cut out of the Signal expression
    val signalValues = ListBuffer.empty[ValDef]
    
    object transformer extends Transformer {
      private def treeTypeNullWarning =
        c.warning(c.enclosingPosition,
            "internal warning: tree type was null, " + 
            "this should not happen but the signal may still work")
      
      private def potentialReactiveConstructionWarning(pos: Position) =
        c.warning(pos,
            "expression should not be placed inside a signal expression " +
            "since it potentially creates a new reactive every time the " +
            "signal is evaluated which can lead to unintentional behavior")
      
      private def isReactive(tree: Tree) =
        if (tree.tpe == null) { treeTypeNullWarning; false }
        else tree.tpe <:< typeOf[Signal[_]] || tree.tpe <:< typeOf[Var[_]]
      
      override def transform(tree: Tree) =
        tree match {
          // pass the SignalSynt argument to every reactive
          // to obtain dynamic dependencies
          //
          // for example, this step transforms
          //   Signal { a() + b() }
          // to
          //   SignalSynt { s => a(s) + b(s) }
          case tree @ Apply(Select(reactive, apply), List())
              if isReactive(reactive) && apply.decoded == "apply" =>
            val reactiveApply = Select(reactive, newTermName("apply"))
            reactiveApply.tpe = tree.tpe
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
          case reactive @ (Apply(_, _) | Select(_, _))
              if isReactive(reactive) &&
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
              !(reactive exists {
                case Apply(Select(chainedReactive, apply), List())
                    if isReactive(chainedReactive) && apply.decoded == "apply" =>
                  def reactiveMethodObjectType(tree: Tree = reactive): Type =
                    if (tree.symbol != reactive.symbol)
                      tree.tpe
                    else if (tree.children.nonEmpty)
                      reactiveMethodObjectType(tree.children.head)
                    else
                      NoType
                  
                  // issue no warning if the reactive is retrieved from a container
                  // determined by the generic type parameter
                  reactiveMethodObjectType() match {
                    case TypeRef(_, _, args) if !(args contains reactive.tpe) =>
                      potentialReactiveConstructionWarning(reactive.pos)
                    case _ =>
                  }
                  
                  true
                case tree: SymTree =>
                  definedSymbols get tree.symbol match {
                    case Some(defTree) if !(reactive exists { _ == defTree }) =>
                      potentialReactiveConstructionWarning(reactive.pos)
                      true
                    case _ => false
                  }
                case _ => false
              }) =>
            
            // create the signal definition to be cut out of the
            // macro expression and its substitution variable
            val signalName = newTermName(c.fresh("s$"))
            
            val signalDef = ValDef(Modifiers(), signalName, TypeTree(), reactive)
            signalValues += signalDef
            
            val ident = Ident(signalName)
            ident.tpe = reactive.tpe
            ident
          
          case _ =>
            super.transform(tree)
        }
    }
    
    val tree = transformer transform expression.tree
    
    // SignalSynt argument function
    val function =
      Function(
        List(
          ValDef(
            Modifiers(), signalSyntArgName,
            TypeTree(weakTypeOf[SignalSynt[A]]), EmptyTree)),
        tree)
    
    // create SignalSynt object
    // use fully-qualified name, so no extra import is needed
    val body =
      Apply(
        TypeApply(
          Select(
            Select(
              Select(
                Ident(nme.ROOTPKG),
                newTermName("react")),
              newTermName("SignalSynt")),
            newTermName("apply")),
          List(TypeTree())),
        List(function))
    
    // assemble the SignalSynt object and the signal values that are accessed
    // by the object, but were cut out of the signal expression during the code
    // transformation
    val block =
      Typed(Block(signalValues.toList, body), TypeTree(weakTypeOf[Signal[A]]))
    
    
//    out.append((c resetLocalAttrs block) + "\n\n")
//    out.close
    
    
    c.Expr[Signal[A]](c resetLocalAttrs block)
  }
}
