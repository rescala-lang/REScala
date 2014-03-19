package `macro`

import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.Context

import react.DepHolder
import react.Signal
import react.SignalSynt

object SignalMacro {
  
  def SignalM[A](expression: A): Signal[A] = macro SignalMacro[A]
  
  def SignalMacro[A: c.WeakTypeTag](c: Context)(expression: c.Expr[A]): c.Expr[Signal[A]] = {
    import c.universe._
    
    
//    val out = new java.io.FileWriter(
//        "/home/pascal/Desktop/debugfile.txt", true)
    
//    out.append(showRaw(reify { { val a = 0; a } }) + "\n\n\n\n")
    
//    extracting sub trees with type Reactive[_]
//    val extractedReactives = expression.tree.filter(t => t.tpe <:< typeOf[DepHolder])
    
    
    // the argument that is used by the SignalSynt class to assemble dynamic dependencies
    // every Signal { ... } macro instance gets expanded into a SignalSynt
    val signalSyntName = newTermName(c.fresh("s$"))
    
    // the signal values that will be pulled out of the Signal expression
    val signalValues = ListBuffer.empty[ValDef]
    
    object transformer extends Transformer {
      override def transform(tree: Tree): Tree =
        tree match {
          // pass the SignalSynt argument to every dependency holder
          // to obtain dynamic dependencies
          //
          // for example, this step transforms
          //   Signal { a() + b() }
          // to
          //   SignalSynt { s => a(s) + b(s) }
          case Apply(Select(depHolder, apply), List())
              if depHolder.tpe <:< typeOf[DepHolder] && apply.decoded == "apply" =>
            super.transform(Apply(Select(depHolder, apply), List(Ident(signalSyntName))))
          
          // pull signal values out of the signal expression, that could potentially
          // create a new signal object for every access
          // it is assumed that such function are pure in the sense that they will
          // create an equivalent signal for each call with the same arguments
          // so the function value has to be calculated just once
          //
          // for example, this step transforms
          //   Signal { event.count() }
          // to
          //   Signal { s() }
          // and creates a signal value
          //   val s = event.count
          case depHolder if depHolder.tpe != null && depHolder.tpe <:< typeOf[DepHolder] =>
            depHolder match {
              // only if the reactive value could be retrieved from function
              // and the object which the function is called on is not a reactive value
              case Apply(_, _) | Select(_, _)
                  if !(depHolder exists {
                        _ match {
                          case Apply(Select(depHolder, apply), List())
                            if depHolder.tpe <:< typeOf[DepHolder] && apply.decoded == "apply" => true
                          case _ => false
                        }
                      }) =>
                
                // if the reactive value is defined in the very same class
                // it is possible that it is not yet constructed
                // so it is simple declared to be lazy-initialized
                val modifiers = depHolder match {
                  case Select(This(_), _) => Modifiers(Flag.LAZY)
                  case _ => Modifiers()
                }
                val signalName = newTermName(c.fresh("s$"))
                val signalDef = ValDef(modifiers, signalName, TypeTree(), depHolder)
                
//                // instead of using the type checker below, inspecting the
//                // symbol and term positions would be a cleaner solution,
//                // but the position does not always give us the range
//                // (but only a single point) which is needed here
//                val macroExpressionLocalIdentifier = depHolder match {
//                  case Select(a @ Ident(_), _) =>
//                    expression.tree.pos.includes(a.symbol.asTerm.pos)
//                  case _ => false
//                }
                
                // ensure that the signal value can be pulled out of the Signal expression
                // by invoking the type-checker to see if it can stand on its own
                // outside the signal expression
                try {
                  c typeCheck (c resetAllAttrs signalDef)
                  signalValues += signalDef
                  Ident(signalName)
                }
                catch {
                  case _: Throwable =>
                    super.transform(tree)
                }
              
              case _ =>
                super.transform(tree)
            }
          
          case _ =>
            super.transform(tree)
        }
    }
    
    val tree = transformer transform expression.tree
    
    // SignalSynt argument function
    val function =
      Function(
        List(ValDef(Modifiers(), signalSyntName, TypeTree(weakTypeOf[SignalSynt[A]]), EmptyTree)),
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
    
    // assemble the SignalSynt object and the signal values that are accessed by the object
    // but were pulled out of the signal expression during the code transformation
    val block = Typed(Block(signalValues.toList, body), TypeTree(weakTypeOf[Signal[A]]))
    
    
//    out.append((c resetAllAttrs block) + "\n\n")
//    out.close
    
    
    c.Expr[Signal[A]](c resetAllAttrs block)
  }
}
