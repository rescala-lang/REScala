package clangast.traversal

import clangast.CASTNode
import clangast.decl.*
import clangast.expr.*
import clangast.expr.unaryop.*
import clangast.expr.binaryop.*
import clangast.stmt.*
import clangast.types.*

trait CASTMapper {
  protected val mapCASTNodeHook: PartialFunction[CASTNode, CASTNode] = PartialFunction.empty

  final def mapCASTNode(node: CASTNode): CASTNode = node match {
    case mapCASTNodeHook(n)   => n.mapChildren(this)
    case cDecl: CDecl         => mapCDecl(cDecl)
    case cExpr: CExpr         => mapCExpr(cExpr)
    case cStmt: CStmt         => mapCStmt(cStmt)
    case cType: CType         => mapCType(cType)
    case cQualType: CQualType => mapCQualType(cQualType)
    case _                    => node.mapChildren(this)
  }

  protected val mapCDeclHook: PartialFunction[CDecl, CDecl] = PartialFunction.empty

  final def mapCDecl(cDecl: CDecl): CDecl = cDecl match {
    case mapCDeclHook(n)                            => n.mapChildren(this)
    case cNamedDecl: CNamedDecl                     => mapCNamedDecl(cNamedDecl)
    case cTranslationUnitDecl: CTranslationUnitDecl => cTranslationUnitDecl.mapChildren(this)
    case _                                          => cDecl.mapChildren(this)
  }

  protected val mapCNamedDeclHook: PartialFunction[CNamedDecl, CNamedDecl] = PartialFunction.empty

  final def mapCNamedDecl(cNamedDecl: CNamedDecl): CNamedDecl = cNamedDecl match {
    case mapCNamedDeclHook(n)   => n.mapChildren(this)
    case cValueDecl: CValueDecl => mapCValueDecl(cValueDecl)
    case cTypeDecl: CTypeDecl   => mapCTypeDecl(cTypeDecl)
    case _                      => cNamedDecl.mapChildren(this)
  }

  protected val mapCValueDeclHook: PartialFunction[CValueDecl, CValueDecl] = PartialFunction.empty

  final def mapCValueDecl(cValueDecl: CValueDecl): CValueDecl = cValueDecl match {
    case mapCValueDeclHook(n)                 => n.mapChildren(this)
    case cEnumConstantDecl: CEnumConstantDecl => mapCEnumConstantDecl(cEnumConstantDecl)
    case cFieldDecl: CFieldDecl               => mapCFieldDecl(cFieldDecl)
    case cParmVarDecl: CParmVarDecl           => mapCParmVarDecl(cParmVarDecl)
    case _                                    => cValueDecl.mapChildren(this)
  }

  protected val mapCEnumConstantDeclHook: PartialFunction[CEnumConstantDecl, CEnumConstantDecl] = PartialFunction.empty

  final def mapCEnumConstantDecl(cEnumConstantDecl: CEnumConstantDecl): CEnumConstantDecl = cEnumConstantDecl match {
    case mapCEnumConstantDeclHook(n) => n.mapChildren(this)
    case _                           => cEnumConstantDecl.mapChildren(this)
  }

  protected val mapCFieldDeclHook: PartialFunction[CFieldDecl, CFieldDecl] = PartialFunction.empty

  final def mapCFieldDecl(cFieldDecl: CFieldDecl): CFieldDecl = cFieldDecl match {
    case mapCFieldDeclHook(n) => n.mapChildren(this)
    case _                    => cFieldDecl.mapChildren(this)
  }

  protected val mapCParmVarDeclHook: PartialFunction[CParmVarDecl, CParmVarDecl] = PartialFunction.empty

  final def mapCParmVarDecl(cParmVarDecl: CParmVarDecl): CParmVarDecl = cParmVarDecl match {
    case mapCParmVarDeclHook(n) => n.mapChildren(this)
    case _                      => cParmVarDecl.mapChildren(this)
  }

  protected val mapCTypeDeclHook: PartialFunction[CTypeDecl, CTypeDecl] = PartialFunction.empty

  final def mapCTypeDecl(cTypeDecl: CTypeDecl): CTypeDecl = cTypeDecl match {
    case mapCTypeDeclHook(n) => n.mapChildren(this)
    case _                   => cTypeDecl.mapChildren(this)
  }

  protected val mapCExprHook: PartialFunction[CExpr, CExpr] = PartialFunction.empty

  final def mapCExpr(cExpr: CExpr): CExpr = cExpr match {
    case mapCExprHook(n)                  => n.mapChildren(this)
    case cBinaryOperator: CBinaryOperator => mapCBinaryOperator(cBinaryOperator)
    case cUnaryOperator: CUnaryOperator   => mapCUnaryOperator(cUnaryOperator)
    case _                                => cExpr.mapChildren(this)
  }

  protected val mapCBinaryOperatorHook: PartialFunction[CBinaryOperator, CBinaryOperator] = PartialFunction.empty

  final def mapCBinaryOperator(cBinaryOperator: CBinaryOperator): CBinaryOperator = cBinaryOperator match {
    case mapCBinaryOperatorHook(n) => n.mapChildren(this)
    case _                         => cBinaryOperator.mapChildren(this)
  }

  protected val mapCUnaryOperatorHook: PartialFunction[CUnaryOperator, CUnaryOperator] = PartialFunction.empty

  final def mapCUnaryOperator(cUnaryOperator: CUnaryOperator): CUnaryOperator = cUnaryOperator match {
    case mapCUnaryOperatorHook(n) => n.mapChildren(this)
    case _                        => cUnaryOperator.mapChildren(this)
  }

  protected val mapCStmtHook: PartialFunction[CStmt, CStmt] = PartialFunction.empty

  final def mapCStmt(cStmt: CStmt): CStmt = cStmt match {
    case mapCStmtHook(n)              => n.mapChildren(this)
    case cSwitchCase: CSwitchCase     => mapCSwitchCase(cSwitchCase)
    case cCompoundStmt: CCompoundStmt => mapCCompoundStmt(cCompoundStmt)
    case _                            => cStmt.mapChildren(this)
  }

  protected val mapCCompoundStmtHook: PartialFunction[CCompoundStmt, CCompoundStmt] = PartialFunction.empty

  final def mapCCompoundStmt(cCompoundStmt: CCompoundStmt): CCompoundStmt = cCompoundStmt match {
    case mapCCompoundStmtHook(n) => n.mapChildren(this)
    case _                       => cCompoundStmt.mapChildren(this)
  }

  protected val mapCSwitchCaseHook: PartialFunction[CSwitchCase, CSwitchCase] = PartialFunction.empty

  final def mapCSwitchCase(cSwitchCase: CSwitchCase): CSwitchCase = cSwitchCase match {
    case mapCSwitchCaseHook(n) => n.mapChildren(this)
    case _                     => cSwitchCase.mapChildren(this)
  }

  protected val mapCTypeHook: PartialFunction[CType, CType] = PartialFunction.empty

  final def mapCType(cType: CType): CType = cType match {
    case mapCTypeHook(n) => n.mapChildren(this)
    case _               => cType.mapChildren(this)
  }

  protected val mapCQualTypeHook: PartialFunction[CQualType, CQualType] = PartialFunction.empty

  final def mapCQualType(cQualType: CQualType): CQualType = cQualType match {
    case mapCQualTypeHook(n) => n.mapChildren(this)
    case _                   => cQualType.mapChildren(this)
  }
}
