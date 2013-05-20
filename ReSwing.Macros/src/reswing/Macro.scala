package reswing

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable.{ListBuffer, Stack}

object Macros {
  def applyBody = macro applyBodyImpl
  def applyBodyImpl(c: Context): c.Expr[Any] = {
    import c.universe._
    
    c.enclosingClass match {
      case ModuleDef(moduleMods, moduleName, moduleImpl) =>
        /*
         * find values that can be overridden
         */
        val typeName = newTypeName(moduleName.encoded)
        val classTypeCheckingExpr = Typed(c.literalNull.tree, Ident(typeName))
        
        val typedTree = c.typeCheck(classTypeCheckingExpr, silent = true)
        if (typedTree.isEmpty)
          c.error(c.enclosingPosition, "Type " + moduleName.decoded + " must be instantiable")
        
        val lazyValues = typedTree.tpe.members
          .collect{ case t: TermSymbol => t }
          .filter{ t => t.isGetter && (t.accessed match {
            case t: TermSymbol => t.isLazy
            case _ => false })
          }
          .map{ t => t.name }
          .toSet
        
        c.enclosingMethod match {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            if (!tparams.isEmpty)
              c.error(c.enclosingPosition, "Generic function parameter not supported")
            
            vparamss match {
              case List(params) =>
                /*
                 * definition of members of the created class
                 * and definitions in the surrounding block
                 */
                val blockDefs = ListBuffer[MemberDef]()
                val classDefs = ListBuffer[MemberDef]()
                val classArgs = ListBuffer[AssignOrNamedArg]()
                
                for (valDef <- params)
                  if (lazyValues.contains(valDef.name)) {
                    val name = newTermName(c.fresh("temp$"))
                    blockDefs += DefDef(Modifiers(), name, List(), List(), TypeTree(), Ident(valDef.name))
                    classDefs += ValDef(Modifiers(Flag.OVERRIDE | Flag.LAZY, tpnme.EMPTY, List()), valDef.name, TypeTree(), Ident(name))
                  }
                  else
                    classArgs += AssignOrNamedArg(Ident(valDef.name), Ident(valDef.name))
                
                /*
                 * definition for anonymous class type $anon that extends the companion class
                 * and contains the previously created member definitions
                 */
                val classCtor = DefDef(
                  Modifiers(),
                  nme.CONSTRUCTOR,
                  List(),
                  List(List()),
                  TypeTree(),
                  Block(
                    List(
                      Apply(
                        Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                        classArgs.toList)),
                    Literal(Constant(()))))
                
                classDefs += classCtor
                
                val classDef = ClassDef(
                  Modifiers(Flag.FINAL, tpnme.EMPTY, List()),
                  newTypeName("$anon"),
                  List(),
                  Template(
                    List(Ident(typeName)),
                    emptyValDef,
                    classDefs.toList))
                
                /*
                 * definition of the block surrounding the anonymous class
                 * that contains the previously created definitions
                 */
                blockDefs += classDef
                  
                val blockDef = Typed(
                  Block(
                    blockDefs.toList,
                    Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())),
                  Ident(typeName))
                
                return c.Expr[Any](blockDef)
                
              case _ => c.error(c.enclosingPosition, "No or multiple parameter lists not supported")
            }
          case _ => c.error(c.enclosingPosition, "Macro must be called inside method")
        }
      case _ => c.error(c.enclosingPosition, "Method must be inside module definition")
    }
    
    // dummy value, we will never end up here and an error has occurred
    c.literalUnit
  }
}
