package clangast.decl

import clangast.types.{CEnumType, CQualType, CType}

import scala.quoted.{Expr, Quotes}

case class CEnumDecl(name: String, integerType: CQualType, enumConstants: List[CEnumConstantDeclaration]) extends CTypeDecl with CDeclContext {
  override def getTypeForDecl: CType = CEnumType(this)
  
  override def decls: List[CDecl] = enumConstants

  override def textgen: String =
    s"enum $name {" + enumConstants.map(_.textgen).mkString(", ") + "}"

  override def toExpr(using Quotes): Expr[CEnumDecl] = {
    val nameExpr = Expr(name)
    val integerTypeExpr = integerType.toExpr
    val enumConstantsExpr = Expr.ofList(enumConstants.map(_.toExpr))

    '{ CEnumDecl($nameExpr, $integerTypeExpr, $enumConstantsExpr) }
  }
}