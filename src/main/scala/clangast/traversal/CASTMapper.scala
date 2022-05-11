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

  final def mapCASTNode(node: CASTNode): CASTNode =
    mapCASTNodeHook.applyOrElse(node, {
      case cDecl: CDecl => mapCDecl(cDecl)
      case cExpr: CExpr => mapCExpr(cExpr)
      case cStmt: CStmt => mapCStmt(cStmt)
      case cType: CType => mapCType(cType)
      case cQualType: CQualType => mapCQualType(cQualType)
      case _ => node.mapChildren(this)
    })

  protected val mapCDeclHook: PartialFunction[CDecl, CDecl] = PartialFunction.empty
  protected val mapCTranslationUnitDeclHook: PartialFunction[CTranslationUnitDecl, CTranslationUnitDecl] = PartialFunction.empty

  final def mapCDecl(cDecl: CDecl): CDecl =
    mapCDeclHook.applyOrElse(cDecl, {
      case cNamedDecl: CNamedDecl => mapCNamedDecl(cNamedDecl)
      case cTranslationUnitDecl: CTranslationUnitDecl =>
        mapCTranslationUnitDeclHook.applyOrElse(cTranslationUnitDecl, _ => cTranslationUnitDecl).mapChildren(this)
      case _ => cDecl.mapChildren(this)
    })

  protected val mapCNamedDeclHook: PartialFunction[CNamedDecl, CNamedDecl] = PartialFunction.empty

  final def mapCNamedDecl(cNamedDecl: CNamedDecl): CNamedDecl =
    mapCNamedDeclHook.applyOrElse(cNamedDecl, {
      case cValueDecl: CValueDecl => mapCValueDecl(cValueDecl)
      case cTypeDecl: CTypeDecl => mapCTypeDecl(cTypeDecl)
      case _ => cNamedDecl.mapChildren(this)
    })

  protected val mapCValueDeclHook: PartialFunction[CValueDecl, CValueDecl] = PartialFunction.empty
  protected val mapCFunctionDeclHook: PartialFunction[CFunctionDecl, CFunctionDecl] = PartialFunction.empty
  protected val mapCVarDeclHook: PartialFunction[CVarDecl, CVarDecl] = PartialFunction.empty

  final def mapCValueDecl(cValueDecl: CValueDecl): CValueDecl =
    mapCValueDeclHook.applyOrElse(cValueDecl, {
      case cEnumConstantDecl: CEnumConstantDecl => mapCEnumConstantDecl(cEnumConstantDecl)
      case cFieldDecl: CFieldDecl => mapCFieldDecl(cFieldDecl)
      case cParmVarDecl: CParmVarDecl => mapCParmVarDecl(cParmVarDecl)
      case cFunctionDecl: CFunctionDecl =>
        mapCFunctionDeclHook.applyOrElse(cFunctionDecl, _ => cFunctionDecl).mapChildren(this)
      case cVarDecl: CVarDecl =>
        mapCVarDeclHook.applyOrElse(cVarDecl, _ => cVarDecl).mapChildren(this)
      case _ => cValueDecl.mapChildren(this)
    })
    
  protected val mapCEnumConstantDeclHook: PartialFunction[CEnumConstantDecl, CEnumConstantDecl] = PartialFunction.empty
  
  final def mapCEnumConstantDecl(cEnumConstantDecl: CEnumConstantDecl): CEnumConstantDecl =
    mapCEnumConstantDeclHook.applyOrElse(cEnumConstantDecl, _ => cEnumConstantDecl).mapChildren(this)

  protected val mapCFieldDeclHook: PartialFunction[CFieldDecl, CFieldDecl] = PartialFunction.empty
  
  final def mapCFieldDecl(cFieldDecl: CFieldDecl): CFieldDecl =
    mapCFieldDeclHook.applyOrElse(cFieldDecl, _ => cFieldDecl).mapChildren(this)

  protected val mapCParmVarDeclHook: PartialFunction[CParmVarDecl, CParmVarDecl] = PartialFunction.empty
  
  final def mapCParmVarDecl(cParmVarDecl: CParmVarDecl): CParmVarDecl =
    mapCParmVarDeclHook.applyOrElse(cParmVarDecl, _ => cParmVarDecl).mapChildren(this)

  protected val mapCTypeDeclHook: PartialFunction[CTypeDecl, CTypeDecl] = PartialFunction.empty
  protected val mapCEnumDeclHook: PartialFunction[CEnumDecl, CEnumDecl] = PartialFunction.empty
  protected val mapCRecordDeclHook: PartialFunction[CRecordDecl, CRecordDecl] = PartialFunction.empty
  protected val mapCTypedefDeclHook: PartialFunction[CTypedefDecl, CTypedefDecl] = PartialFunction.empty

  final def mapCTypeDecl(cTypeDecl: CTypeDecl): CTypeDecl =
    mapCTypeDeclHook.applyOrElse(cTypeDecl, {
      case cEnumDecl: CEnumDecl =>
        mapCEnumDeclHook.applyOrElse(cEnumDecl, _ => cEnumDecl).mapChildren(this)
      case cRecordDecl: CRecordDecl =>
        mapCRecordDeclHook.applyOrElse(cRecordDecl, _ => cRecordDecl).mapChildren(this)
      case cTypedefDecl: CTypedefDecl =>
        mapCTypedefDeclHook.applyOrElse(cTypedefDecl, _ => cTypedefDecl).mapChildren(this)
      case _ => cTypeDecl.mapChildren(this)
    })

  protected val mapCExprHook: PartialFunction[CExpr, CExpr] = PartialFunction.empty
  protected val mapCArraySubscriptExprHook: PartialFunction[CArraySubscriptExpr, CArraySubscriptExpr] = PartialFunction.empty
  protected val mapCCallExprHook: PartialFunction[CCallExpr, CCallExpr] = PartialFunction.empty
  protected val mapCCastExprHook: PartialFunction[CCastExpr, CCastExpr] = PartialFunction.empty
  protected val mapCCharacterLiteralHook: PartialFunction[CCharacterLiteral, CCharacterLiteral] = PartialFunction.empty
  protected val mapCConditionalOperatorHook: PartialFunction[CConditionalOperator, CConditionalOperator] = PartialFunction.empty
  protected val mapCDeclRefExprHook: PartialFunction[CDeclRefExpr, CDeclRefExpr] = PartialFunction.empty
  protected val mapCDesignatedInitExprHook: PartialFunction[CDesignatedInitExpr, CDesignatedInitExpr] = PartialFunction.empty
  protected val mapCDoubleLiteralHook: PartialFunction[CDoubleLiteral, CDoubleLiteral] = PartialFunction.empty
  protected val mapCFloatLiteralHook: PartialFunction[CFloatLiteral, CFloatLiteral] = PartialFunction.empty
  protected val mapCInitListExprHook: PartialFunction[CInitListExpr, CInitListExpr] = PartialFunction.empty
  protected val mapCIntegerLiteralHook: PartialFunction[CIntegerLiteral, CIntegerLiteral] = PartialFunction.empty
  protected val mapCLongLiteralHook: PartialFunction[CLongLiteral, CLongLiteral] = PartialFunction.empty
  protected val mapCMemberExprHook: PartialFunction[CMemberExpr, CMemberExpr] = PartialFunction.empty
  protected val mapCParenExprHook: PartialFunction[CParenExpr, CParenExpr] = PartialFunction.empty
  protected val mapCSizeofExprHook: PartialFunction[CSizeofExpr, CSizeofExpr] = PartialFunction.empty
  protected val mapCStmtExprHook: PartialFunction[CStmtExpr, CStmtExpr] = PartialFunction.empty
  protected val mapCStringLiteralHook: PartialFunction[CStringLiteral, CStringLiteral] = PartialFunction.empty

  final def mapCExpr(cExpr: CExpr): CExpr =
    mapCExprHook.applyOrElse(cExpr, {
      case cBinaryOperator: CBinaryOperator => mapCBinaryOperator(cBinaryOperator)
      case cUnaryOperator: CUnaryOperator => mapCUnaryOperator(cUnaryOperator)
      case cArraySubscriptExpr: CArraySubscriptExpr =>
        mapCArraySubscriptExprHook.applyOrElse(cArraySubscriptExpr, _ => cArraySubscriptExpr).mapChildren(this)
      case cCallExpr: CCallExpr =>
        mapCCallExprHook.applyOrElse(cCallExpr, _ => cCallExpr).mapChildren(this)
      case cCastExpr: CCastExpr =>
        mapCCastExprHook.applyOrElse(cCastExpr, _ => cCastExpr).mapChildren(this)
      case cCharacterLiteral: CCharacterLiteral =>
        mapCCharacterLiteralHook.applyOrElse(cCharacterLiteral, _ => cCharacterLiteral)
      case cConditionalOperator: CConditionalOperator =>
        mapCConditionalOperatorHook.applyOrElse(cConditionalOperator, _ => cConditionalOperator).mapChildren(this)
      case cDeclRefExpr: CDeclRefExpr =>
        mapCDeclRefExprHook.applyOrElse(cDeclRefExpr, _ => cDeclRefExpr)
      case cDesignatedInitExpr: CDesignatedInitExpr =>
        mapCDesignatedInitExprHook.applyOrElse(cDesignatedInitExpr, _ => cDesignatedInitExpr).mapChildren(this)
      case cDoubleLiteral: CDoubleLiteral =>
        mapCDoubleLiteralHook.applyOrElse(cDoubleLiteral, _ => cDoubleLiteral)
      case cFloatLiteral: CFloatLiteral =>
        mapCFloatLiteralHook.applyOrElse(cFloatLiteral, _ => cFloatLiteral)
      case cInitListExpr: CInitListExpr =>
        mapCInitListExprHook.applyOrElse(cInitListExpr, _ => cInitListExpr).mapChildren(this)
      case cIntegerLiteral: CIntegerLiteral =>
        mapCIntegerLiteralHook.applyOrElse(cIntegerLiteral, _ => cIntegerLiteral)
      case cLongLiteral: CLongLiteral =>
        mapCLongLiteralHook.applyOrElse(cLongLiteral, _ => cLongLiteral)
      case cMemberExpr: CMemberExpr =>
        mapCMemberExprHook.applyOrElse(cMemberExpr, _ => cMemberExpr).mapChildren(this)
      case cParenExpr: CParenExpr =>
        mapCParenExprHook.applyOrElse(cParenExpr, _ => cParenExpr).mapChildren(this)
      case cSizeofExpr: CSizeofExpr =>
        mapCSizeofExprHook.applyOrElse(cSizeofExpr, _ => cSizeofExpr).mapChildren(this)
      case cStmtExpr: CStmtExpr =>
        mapCStmtExprHook.applyOrElse(cStmtExpr, _ => cStmtExpr).mapChildren(this)
      case cStringLiteral: CStringLiteral =>
        mapCStringLiteralHook.applyOrElse(cStringLiteral, _ => cStringLiteral)
      case _ => cExpr.mapChildren(this)
    })

  protected val mapCBinaryOperatorHook: PartialFunction[CBinaryOperator, CBinaryOperator] = PartialFunction.empty
  protected val mapCAndExprHook: PartialFunction[CAndExpr, CAndExpr] = PartialFunction.empty
  protected val mapCAssignmentExprHook: PartialFunction[CAssignmentExpr, CAssignmentExpr] = PartialFunction.empty
  protected val mapCBitwiseAndAssignmentExprHook: PartialFunction[CBitwiseAndAssignmentExpr, CBitwiseAndAssignmentExpr] = PartialFunction.empty
  protected val mapCBitwiseAndExprHook: PartialFunction[CBitwiseAndExpr, CBitwiseAndExpr] = PartialFunction.empty
  protected val mapCBitwiseOrAssignmentExprHook: PartialFunction[CBitwiseOrAssignmentExpr, CBitwiseOrAssignmentExpr] = PartialFunction.empty
  protected val mapCBitwiseOrExprHook: PartialFunction[CBitwiseOrExpr, CBitwiseOrExpr] = PartialFunction.empty
  protected val mapCBitwiseXorAssignmentExprHook: PartialFunction[CBitwiseXorAssignmentExpr, CBitwiseXorAssignmentExpr] = PartialFunction.empty
  protected val mapCBitwiseXorExprHook: PartialFunction[CBitwiseXorExpr, CBitwiseXorExpr] = PartialFunction.empty
  protected val mapCDivAssignmentExprHook: PartialFunction[CDivAssignmentExpr, CDivAssignmentExpr] = PartialFunction.empty
  protected val mapCDivExprHook: PartialFunction[CDivExpr, CDivExpr] = PartialFunction.empty
  protected val mapCEqualsExprHook: PartialFunction[CEqualsExpr, CEqualsExpr] = PartialFunction.empty
  protected val mapCGreaterEqualsExprHook: PartialFunction[CGreaterEqualsExpr, CGreaterEqualsExpr] = PartialFunction.empty
  protected val mapCGreaterThanExprHook: PartialFunction[CGreaterThanExpr, CGreaterThanExpr] = PartialFunction.empty
  protected val mapCLeftShiftAssignmentExprHook: PartialFunction[CLeftShiftAssignmentExpr, CLeftShiftAssignmentExpr] = PartialFunction.empty
  protected val mapCLeftShiftExprHook: PartialFunction[CLeftShiftExpr, CLeftShiftExpr] = PartialFunction.empty
  protected val mapCLessEqualsExprHook: PartialFunction[CLessEqualsExpr, CLessEqualsExpr] = PartialFunction.empty
  protected val mapCLessThanExprHook: PartialFunction[CLessThanExpr, CLessThanExpr] = PartialFunction.empty
  protected val mapCMinusAssignmentExprHook: PartialFunction[CMinusAssignmentExpr, CMinusAssignmentExpr] = PartialFunction.empty
  protected val mapCMinusExprHook: PartialFunction[CMinusExpr, CMinusExpr] = PartialFunction.empty
  protected val mapCModAssignmentExprHook: PartialFunction[CModAssignmentExpr, CModAssignmentExpr] = PartialFunction.empty
  protected val mapCModExprHook: PartialFunction[CModExpr, CModExpr] = PartialFunction.empty
  protected val mapCNotEqualsExprHook: PartialFunction[CNotEqualsExpr, CNotEqualsExpr] = PartialFunction.empty
  protected val mapCOrExprHook: PartialFunction[COrExpr, COrExpr] = PartialFunction.empty
  protected val mapCPlusAssignmentExprHook: PartialFunction[CPlusAssignmentExpr, CPlusAssignmentExpr] = PartialFunction.empty
  protected val mapCPlusExprHook: PartialFunction[CPlusExpr, CPlusExpr] = PartialFunction.empty
  protected val mapCProdAssignmentExprHook: PartialFunction[CProdAssignmentExpr, CProdAssignmentExpr] = PartialFunction.empty
  protected val mapCProdExprHook: PartialFunction[CProdExpr, CProdExpr] = PartialFunction.empty
  protected val mapCRightShiftAssignmentExprHook: PartialFunction[CRightShiftAssignmentExpr, CRightShiftAssignmentExpr] = PartialFunction.empty
  protected val mapCRightShiftExprHook: PartialFunction[CRightShiftExpr, CRightShiftExpr] = PartialFunction.empty

  final def mapCBinaryOperator(cBinaryOperator: CBinaryOperator): CBinaryOperator =
    mapCBinaryOperatorHook.applyOrElse(cBinaryOperator, {
      case cAndExpr: CAndExpr =>
        mapCAndExprHook.applyOrElse(cAndExpr, _ => cAndExpr).mapChildren(this)
      case cAssignmentExpr: CAssignmentExpr =>
        mapCAssignmentExprHook.applyOrElse(cAssignmentExpr, _ => cAssignmentExpr).mapChildren(this)
      case cBitwiseAndAssignmentExpr: CBitwiseAndAssignmentExpr =>
        mapCBitwiseAndAssignmentExprHook.applyOrElse(cBitwiseAndAssignmentExpr, _ => cBitwiseAndAssignmentExpr).mapChildren(this)
      case cBitwiseAndExpr: CBitwiseAndExpr =>
        mapCBitwiseAndExprHook.applyOrElse(cBitwiseAndExpr, _ => cBitwiseAndExpr).mapChildren(this)
      case cBitwiseOrAssignmentExpr: CBitwiseOrAssignmentExpr =>
        mapCBitwiseOrAssignmentExprHook.applyOrElse(cBitwiseOrAssignmentExpr, _ => cBitwiseOrAssignmentExpr).mapChildren(this)
      case cBitwiseOrExpr: CBitwiseOrExpr =>
        mapCBitwiseOrExprHook.applyOrElse(cBitwiseOrExpr, _ => cBitwiseOrExpr).mapChildren(this)
      case cBitwiseXorAssignmentExpr: CBitwiseXorAssignmentExpr =>
        mapCBitwiseXorAssignmentExprHook.applyOrElse(cBitwiseXorAssignmentExpr, _ => cBitwiseXorAssignmentExpr).mapChildren(this)
      case cBitwiseXorExpr: CBitwiseXorExpr =>
        mapCBitwiseXorExprHook.applyOrElse(cBitwiseXorExpr, _ => cBitwiseXorExpr).mapChildren(this)
      case cDivAssignmentExpr: CDivAssignmentExpr =>
        mapCDivAssignmentExprHook.applyOrElse(cDivAssignmentExpr, _ => cDivAssignmentExpr).mapChildren(this)
      case cDivExpr: CDivExpr =>
        mapCDivExprHook.applyOrElse(cDivExpr, _ => cDivExpr).mapChildren(this)
      case cEqualsExpr: CEqualsExpr =>
        mapCEqualsExprHook.applyOrElse(cEqualsExpr, _ => cEqualsExpr).mapChildren(this)
      case cGreaterEqualsExpr: CGreaterEqualsExpr =>
        mapCGreaterEqualsExprHook.applyOrElse(cGreaterEqualsExpr, _ => cGreaterEqualsExpr).mapChildren(this)
      case cGreaterThanExpr: CGreaterThanExpr =>
        mapCGreaterThanExprHook.applyOrElse(cGreaterThanExpr, _ => cGreaterThanExpr).mapChildren(this)
      case cLeftShiftAssignmentExpr: CLeftShiftAssignmentExpr =>
        mapCLeftShiftAssignmentExprHook.applyOrElse(cLeftShiftAssignmentExpr, _ => cLeftShiftAssignmentExpr).mapChildren(this)
      case cLeftShiftExpr: CLeftShiftExpr =>
        mapCLeftShiftExprHook.applyOrElse(cLeftShiftExpr, _ => cLeftShiftExpr).mapChildren(this)
      case cLessEqualsExpr: CLessEqualsExpr =>
        mapCLessEqualsExprHook.applyOrElse(cLessEqualsExpr, _ => cLessEqualsExpr).mapChildren(this)
      case cLessThanExpr: CLessThanExpr =>
        mapCLessThanExprHook.applyOrElse(cLessThanExpr, _ => cLessThanExpr).mapChildren(this)
      case cMinusAssignmentExpr: CMinusAssignmentExpr =>
        mapCMinusAssignmentExprHook.applyOrElse(cMinusAssignmentExpr, _ => cMinusAssignmentExpr).mapChildren(this)
      case cMinusExpr: CMinusExpr =>
        mapCMinusExprHook.applyOrElse(cMinusExpr, _ => cMinusExpr).mapChildren(this)
      case cModAssignmentExpr: CModAssignmentExpr =>
        mapCModAssignmentExprHook.applyOrElse(cModAssignmentExpr, _ => cModAssignmentExpr).mapChildren(this)
      case cModExpr: CModExpr =>
        mapCModExprHook.applyOrElse(cModExpr, _ => cModExpr).mapChildren(this)
      case cNotEqualsExpr: CNotEqualsExpr =>
        mapCNotEqualsExprHook.applyOrElse(cNotEqualsExpr, _ => cNotEqualsExpr).mapChildren(this)
      case cOrExpr: COrExpr =>
        mapCOrExprHook.applyOrElse(cOrExpr, _ => cOrExpr).mapChildren(this)
      case cPlusAssignmentExpr: CPlusAssignmentExpr =>
        mapCPlusAssignmentExprHook.applyOrElse(cPlusAssignmentExpr, _ => cPlusAssignmentExpr).mapChildren(this)
      case cPlusExpr: CPlusExpr =>
        mapCPlusExprHook.applyOrElse(cPlusExpr, _ => cPlusExpr).mapChildren(this)
      case cProdAssignmentExpr: CProdAssignmentExpr =>
        mapCProdAssignmentExprHook.applyOrElse(cProdAssignmentExpr, _ => cProdAssignmentExpr).mapChildren(this)
      case cProdExpr: CProdExpr =>
        mapCProdExprHook.applyOrElse(cProdExpr, _ => cProdExpr).mapChildren(this)
      case cRightShiftAssignmentExpr: CRightShiftAssignmentExpr =>
        mapCRightShiftAssignmentExprHook.applyOrElse(cRightShiftAssignmentExpr, _ => cRightShiftAssignmentExpr).mapChildren(this)
      case cRightShiftExpr: CRightShiftExpr =>
        mapCRightShiftExprHook.applyOrElse(cRightShiftExpr, _ => cRightShiftExpr).mapChildren(this)
      case _ => cBinaryOperator.mapChildren(this)
    })

  protected val mapCUnaryOperatorHook: PartialFunction[CUnaryOperator, CUnaryOperator] = PartialFunction.empty
  protected val mapCAddressExprHook: PartialFunction[CAddressExpr, CAddressExpr] = PartialFunction.empty
  protected val mapCBitwiseNotExprHook: PartialFunction[CBitwiseNotExpr, CBitwiseNotExpr] = PartialFunction.empty
  protected val mapCDecExprHook: PartialFunction[CDecExpr, CDecExpr] = PartialFunction.empty
  protected val mapCDerefExprHook: PartialFunction[CDerefExpr, CDerefExpr] = PartialFunction.empty
  protected val mapCIncExprHook: PartialFunction[CIncExpr, CIncExpr] = PartialFunction.empty
  protected val mapCNotExprHook: PartialFunction[CNotExpr, CNotExpr] = PartialFunction.empty
  protected val mapCUnaryMinusExprHook: PartialFunction[CUnaryMinusExpr, CUnaryMinusExpr] = PartialFunction.empty
  protected val mapCUnaryPlusExprHook: PartialFunction[CUnaryPlusExpr, CUnaryPlusExpr] = PartialFunction.empty

  final def mapCUnaryOperator(cUnaryOperator: CUnaryOperator): CUnaryOperator =
    mapCUnaryOperatorHook.applyOrElse(cUnaryOperator, {
      case cAddressExpr: CAddressExpr =>
        mapCAddressExprHook.applyOrElse(cAddressExpr, _ => cAddressExpr).mapChildren(this)
      case cBitwiseNotExpr: CBitwiseNotExpr =>
        mapCBitwiseNotExprHook.applyOrElse(cBitwiseNotExpr, _ => cBitwiseNotExpr).mapChildren(this)
      case cDecExpr: CDecExpr =>
        mapCDecExprHook.applyOrElse(cDecExpr, _ => cDecExpr).mapChildren(this)
      case cDerefExpr: CDerefExpr =>
        mapCDerefExprHook.applyOrElse(cDerefExpr, _ => cDerefExpr).mapChildren(this)
      case cIncExpr: CIncExpr =>
        mapCIncExprHook.applyOrElse(cIncExpr, _ => cIncExpr).mapChildren(this)
      case cNotExpr: CNotExpr =>
        mapCNotExprHook.applyOrElse(cNotExpr, _ => cNotExpr).mapChildren(this)
      case cUnaryMinusExpr: CUnaryMinusExpr =>
        mapCUnaryMinusExprHook.applyOrElse(cUnaryMinusExpr, _ => cUnaryMinusExpr).mapChildren(this)
      case cUnaryPlusExpr: CUnaryPlusExpr =>
        mapCUnaryPlusExprHook.applyOrElse(cUnaryPlusExpr, _ => cUnaryPlusExpr).mapChildren(this)
      case _ => cUnaryOperator.mapChildren(this)
    })

  protected val mapCStmtHook: PartialFunction[CStmt, CStmt] = PartialFunction.empty
  protected val mapCDeclStmtHook: PartialFunction[CDeclStmt, CDeclStmt] = PartialFunction.empty
  protected val mapCDoStmtHook: PartialFunction[CDoStmt, CDoStmt] = PartialFunction.empty
  protected val mapCExprStmtHook: PartialFunction[CExprStmt, CExprStmt] = PartialFunction.empty
  protected val mapCForStmtHook: PartialFunction[CForStmt, CForStmt] = PartialFunction.empty
  protected val mapCIfStmtHook: PartialFunction[CIfStmt, CIfStmt] = PartialFunction.empty
  protected val mapCReturnStmtHook: PartialFunction[CReturnStmt, CReturnStmt] = PartialFunction.empty
  protected val mapCSwitchStmtHook: PartialFunction[CSwitchStmt, CSwitchStmt] = PartialFunction.empty
  protected val mapCWhileStmtHook: PartialFunction[CWhileStmt, CWhileStmt] = PartialFunction.empty

  final def mapCStmt(cStmt: CStmt): CStmt =
    mapCStmtHook.applyOrElse(cStmt, {
      case cSwitchCase: CSwitchCase => mapCSwitchCase(cSwitchCase)
      case cCompoundStmt: CCompoundStmt => mapCCompoundStmt(cCompoundStmt)
      case cDeclStmt: CDeclStmt =>
        mapCDeclStmtHook.applyOrElse(cDeclStmt, _ => cDeclStmt).mapChildren(this)
      case cDoStmt: CDoStmt =>
        mapCDoStmtHook.applyOrElse(cDoStmt, _ => cDoStmt).mapChildren(this)
      case cExprStmt: CExprStmt =>
        mapCExprStmtHook.applyOrElse(cExprStmt, _ => cExprStmt).mapChildren(this)
      case cForStmt: CForStmt =>
        mapCForStmtHook.applyOrElse(cForStmt, _ => cForStmt).mapChildren(this)
      case cIfStmt: CIfStmt =>
        mapCIfStmtHook.applyOrElse(cIfStmt, _ => cIfStmt).mapChildren(this)
      case cReturnStmt: CReturnStmt =>
        mapCReturnStmtHook.applyOrElse(cReturnStmt, _ => cReturnStmt).mapChildren(this)
      case cSwitchStmt: CSwitchStmt =>
        mapCSwitchStmtHook.applyOrElse(cSwitchStmt, _ => cSwitchStmt).mapChildren(this)
      case cWhileStmt: CWhileStmt =>
        mapCWhileStmtHook.applyOrElse(cWhileStmt, _ => cWhileStmt).mapChildren(this)
      case _ => cStmt.mapChildren(this)
    })

  protected val mapCCompoundStmtHook: PartialFunction[CCompoundStmt, CCompoundStmt] = PartialFunction.empty
  
  final def mapCCompoundStmt(cCompoundStmt: CCompoundStmt): CCompoundStmt =
    mapCCompoundStmtHook.applyOrElse(cCompoundStmt, _ => cCompoundStmt).mapChildren(this)

  protected val mapCSwitchCaseHook: PartialFunction[CSwitchCase, CSwitchCase] = PartialFunction.empty
  protected val mapCCaseStmtHook: PartialFunction[CCaseStmt, CCaseStmt] = PartialFunction.empty
  protected val mapCDefaultStmtHook: PartialFunction[CDefaultStmt, CDefaultStmt] = PartialFunction.empty

  final def mapCSwitchCase(cSwitchCase: CSwitchCase): CSwitchCase =
    mapCSwitchCaseHook.applyOrElse(cSwitchCase, {
      case cCaseStmt: CCaseStmt =>
        mapCCaseStmtHook.applyOrElse(cCaseStmt, _ => cCaseStmt).mapChildren(this)
      case cDefaultStmt: CDefaultStmt =>
        mapCDefaultStmtHook.applyOrElse(cDefaultStmt, _ => cDefaultStmt).mapChildren(this)
      case _ => cSwitchCase.mapChildren(this)
    })

  protected val mapCTypeHook: PartialFunction[CType, CType] = PartialFunction.empty
  protected val mapCArrayTypeHook: PartialFunction[CArrayType, CArrayType] = PartialFunction.empty
  protected val mapCBuiltinTypeHook: PartialFunction[CBuiltinType, CBuiltinType] = PartialFunction.empty
  protected val mapCEnumTypeHook: PartialFunction[CEnumType, CEnumType] = PartialFunction.empty
  protected val mapCFunctionTypeHook: PartialFunction[CFunctionType, CFunctionType] = PartialFunction.empty
  protected val mapCPointerTypeHook: PartialFunction[CPointerType, CPointerType] = PartialFunction.empty
  protected val mapCRecordTypeHook: PartialFunction[CRecordType, CRecordType] = PartialFunction.empty
  protected val mapCTypedefTypeHook: PartialFunction[CTypedefType, CTypedefType] = PartialFunction.empty

  final def mapCType(cType: CType): CType =
    mapCTypeHook.applyOrElse(cType, {
      case cArrayType: CArrayType =>
        mapCArrayTypeHook.applyOrElse(cArrayType, _ => cArrayType).mapChildren(this)
      case cBuiltinType: CBuiltinType =>
        mapCBuiltinTypeHook.applyOrElse(cBuiltinType, _ => cBuiltinType)
      case cEnumType: CEnumType =>
        mapCEnumTypeHook.applyOrElse(cEnumType, _ => cEnumType)
      case cFunctionType: CFunctionType =>
        mapCFunctionTypeHook.applyOrElse(cFunctionType, _ => cFunctionType).mapChildren(this)
      case cPointerType: CPointerType =>
        mapCPointerTypeHook.applyOrElse(cPointerType, _ => cPointerType).mapChildren(this)
      case cRecordType: CRecordType =>
        mapCRecordTypeHook.applyOrElse(cRecordType, _ => cRecordType)
      case cTypedefType: CTypedefType =>
        mapCTypedefTypeHook.applyOrElse(cTypedefType, _ => cTypedefType)
      case _ => cType.mapChildren(this)
    })

  protected val mapCQualTypeHook: PartialFunction[CQualType, CQualType] = PartialFunction.empty
  
  final def mapCQualType(cQualType: CQualType): CQualType =
    mapCQualTypeHook.applyOrElse(cQualType, _ => cQualType).mapChildren(this)
}
