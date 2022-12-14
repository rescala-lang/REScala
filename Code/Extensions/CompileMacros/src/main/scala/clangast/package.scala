import clangast.decl.*
import clangast.expr.*
// import clangast.expr.binaryop.*
import clangast.stmt.*
import clangast.types.*

import scala.annotation.targetName

import scala.quoted.*

package object clangast {
  given Conversion[CType, CQualType] = CQualType(_)
  given Conversion[CDecl, CStmt]     = CDeclStmt(_)
  given Conversion[CExpr, CStmt]     = CExprStmt(_)

  /*  extension (lhs: CExpr)
    @targetName("plus")
    infix def +(rhs: CExpr) = CPlusExpr(lhs, rhs)
    @targetName("minus")
    infix def -(rhs: CExpr) = CMinusExpr(lhs, rhs)
    @targetName("prod")
    infix def *(rhs: CExpr) = CProdExpr(lhs, rhs)
    @targetName("div")
    infix def /(rhs: CExpr) = CDivExpr(lhs, rhs)
    @targetName("mod")
    infix def %(rhs: CExpr) = CModExpr(lhs, rhs)*/

  extension (i: Int)
    def lit: CIntegerLiteral = CIntegerLiteral(i)

  extension [T](o: Option[Expr[T]])
    def toExpr(using Quotes, Type[T]): Expr[Option[T]] = o match {
      case None       => '{ None }
      case Some(expr) => '{ Some($expr) }
    }
}
