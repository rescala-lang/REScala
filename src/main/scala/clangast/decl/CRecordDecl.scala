package clangast.decl
import clangast.traversal.CASTMapper
import clangast.types.{CRecordType, CType}

import scala.quoted.{Expr, Quotes}

case class CRecordDecl(name: String, fields: List[CFieldDecl]) extends CTypeDecl with CDeclContext {
  override def getTypeForDecl: CType = CRecordType(this)

  override def decls: List[CDecl] = fields

  override def textgen: String = {
    s"""
       |typedef struct {
       |  ${fields.map(_.textgen).mkString("\n  ")}
       |} $name;
    """.strip().stripMargin
  }

  override def toExpr(using Quotes): Expr[CRecordDecl] = {
    val nameExpr = Expr(name)
    val fieldsExpr = Expr.ofList(fields.map(_.toExpr))

    '{ CRecordDecl($nameExpr, $fieldsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CRecordDecl =
    CRecordDecl(name, fields.map(mapper.mapCFieldDecl))
}
