package macro

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
    
    val signalSyntName = newTermName(c.fresh("s$"))
    val signalValues = ListBuffer.empty[ValDef]
    
    object transformer extends Transformer {
      override def transform(tree: Tree): Tree =
        tree match {
          case Apply(Select(depHolder, apply), List())
              if depHolder.tpe <:< typeOf[DepHolder] && apply.decoded == "apply" =>
            Apply(super.transform(Select(depHolder, newTermName("apply"))), List(Ident(signalSyntName)))
          
          case depHolder if depHolder.tpe <:< typeOf[DepHolder] =>
            depHolder match {
              case Apply(_, args) =>
                if (args exists { _ exists { _.isInstanceOf[IdentContextApi] } })
                  c.error(depHolder.pos, "Another Signal cannot be used inside a Signal expression " +
                                         "if it is depends on local values")
                
                val signalName = newTermName(c.fresh("s$"))
                signalValues += ValDef(Modifiers(), signalName, TypeTree(), depHolder)
                Ident(signalName)
              case _ =>
                super.transform(tree)
            }
          
          case _ =>
            super.transform(tree)
        }
    }
    
    val tree = transformer transform expression.tree
    
    val function =
      Function(
        List(ValDef(Modifiers(), signalSyntName, TypeTree(weakTypeOf[SignalSynt[A]]), EmptyTree)),
        tree)
    
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
    
    val block = Typed(Block(signalValues.toList, body), TypeTree(weakTypeOf[Signal[A]]))
    
//    out.append((c resetAllAttrs block) + "\n\n")
//    out.close
    
    c.Expr[Signal[A]](c resetAllAttrs block)
  }
}
