package macro


import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

import react._

import scala.collection.mutable.ListBuffer



object SignalMacro {

  def SignalXXX[A](expression: A): Signal[A] = macro Signal_XXX[A]

  def Signal_XXX[A: c.WeakTypeTag](c: Context)(expression: c.Expr[A]): c.Expr[SignalSynt[A]] = {
    import c.universe._
    

    //val out = new java.io.FileWriter(
    //    "/home/guido/Desktop/darmstadt/NREScala_workspace/GuidoScalaReactiveMacro/debugfile.txt",true)

    // extracting sub trees with type Reactive[_]
    val extractedReactives = expression.tree.filter(t => t.tpe <:< typeOf[DepHolder])
    
    val sName = newTermName(c.fresh("s$"))

    object tracingTransformer extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case Apply(
                 Select(x,y: TermName), 
                 List()
                ) if y.decoded == "apply" &&
                  x.tpe <:< typeOf[DepHolder]
          =>      
            val newTree = Apply(
                 Select(x,y), 
                 List( c.Expr[Signal[A]](Ident(sName)).tree )
                )
            // out.append("Intermediate:  " + ru.showRaw(newTree) + "\n")
            //out.write(ru.show(y.decoded ) + "\n")
            super.transform(tree)
            //val result = super.transform(tree)
            newTree
          case _ =>
            super.transform(tree)
        }
      }
    }

    val t = tracingTransformer.transform(expression.tree)
    //out.append("Manip:  " + ru.showRaw(c.Expr[A](c.resetAllAttrs(t)).splice) + "\n")	

    val function = Function(
      List(ValDef(Modifiers(), sName, TypeTree(weakTypeOf[SignalSynt[A]]), EmptyTree)),
      t)

    val body = reify {
      SignalSynt[A]() { c.Expr[SignalSynt[A] => A](function).splice }
    }.tree             
                 
                 
   // out.append(" --> " + ru.showRaw(body) + "\n")
  
    //out.close

    c.Expr[SignalSynt[A]](c.resetAllAttrs(body))
    
  }

}




