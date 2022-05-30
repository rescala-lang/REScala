package macros

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.CNotEqualsExpr
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.*
import clangast.types.*
import macros.CompileTerm.compileTermToCExpr
import macros.CompileType.*

import scala.quoted.*

object CompileProduct {
  def compileApply(using Quotes)(ctx: TranslationContext): PartialFunction[quotes.reflect.Apply, CExpr] = apply => {
    import quotes.reflect.*

    apply match {
      case Apply(Select(_, "apply"), l) if isProductApply(apply) =>
        CCallExpr(CDeclRefExpr(getProductCreator(apply.tpe, ctx)), l.map(compileTermToCExpr(_, ctx)))
      case Apply(TypeApply(Select(_, "apply"), _), l) if isProductApply(apply) =>
        CCallExpr(CDeclRefExpr(getProductCreator(apply.tpe, ctx)), l.map(compileTermToCExpr(_, ctx)))
      case Apply(Select(left, "=="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
        CCallExpr(
          CDeclRefExpr(getProductEquals(left.tpe, ctx)),
          List(
            compileTermToCExpr(left, ctx),
            compileTermToCExpr(right, ctx)
          )
        )
      case Apply(Select(left, "!="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
        CNotExpr(
          CCallExpr(
            CDeclRefExpr(getProductEquals(left.tpe, ctx)),
            List(
              compileTermToCExpr(left, ctx),
              compileTermToCExpr(right, ctx)
            )
          )
        )
    }
  }

  def isProductApply(using Quotes)(apply: quotes.reflect.Apply): Boolean = {
    import quotes.reflect.*

    apply match {
      case Apply(Select(i, "apply"), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case Apply(TypeApply(Select(i, "apply"), _), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case _ => false
    }
  }

  def compileSelect(using Quotes)(ctx: TranslationContext): PartialFunction[quotes.reflect.Select, CExpr] = select => {
    import quotes.reflect.*
    
    select match {
      case Select(qualifier, name) if isProductFieldAccess(qualifier, name) =>
        val recordDecl = getProductRecordDecl(qualifier.tpe, ctx)
        
        CMemberExpr(compileTermToCExpr(qualifier, ctx), recordDecl.fields.find(_.name.equals(name)).get)
    }
  }

  def isProductFieldAccess(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    (term.tpe <:< TypeRepr.of[Product]) && term.tpe.classSymbol.get.caseFields.exists(_.name.equals(name))
  }

  def compileValDefToCVarDecl(using Quotes)(ctx: TranslationContext): PartialFunction[quotes.reflect.ValDef, CVarDecl] = valDef => {
    import quotes.reflect.*
    
    valDef match {
      case ValDef(name, tpt, Some(apply@Apply(Select(_, "apply"), _))) if isProductApply(apply) =>
        CVarDecl(name, compileTypeRepr(tpt.tpe, ctx), Some(compileProductApplyToCDesignatedInitExpr(apply, ctx)))
    }
  }

  def compileProductApplyToCDesignatedInitExpr(using Quotes)(apply: quotes.reflect.Apply, ctx: TranslationContext): CDesignatedInitExpr = {
    import quotes.reflect.*

    apply match {
      case Apply(Select(_, "apply"), l) if isProductApply(apply) =>
        val recordDecl = getProductRecordDecl(apply.tpe, ctx)
        CDesignatedInitExpr(recordDecl.fields.map(_.name) zip l.map(compileTermToCExpr(_, ctx)))
    }
  }

  def getProductCreator(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl = {
    ctx.nameToRecordCreator.getOrElseUpdate(typeName(tpe), buildProductCreator(getProductRecordDecl(tpe, ctx), ctx))
  }

  def buildProductCreator(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    val name = "create_" + recordDecl.name

    val parameters = recordDecl.fields.map {
      case CFieldDecl(name, declaredType) => CParmVarDecl(name, declaredType)
    }

    val returnType = CRecordType(recordDecl)

    val temp = CVarDecl(
      "temp",
      CRecordType(recordDecl),
      Some(CDesignatedInitExpr(
        parameters.map{ p => (p.name, CDeclRefExpr(p)) }
      ))
    )
    val body = CCompoundStmt(List(
      temp,
      CReturnStmt(Some(CDeclRefExpr(temp)))
    ))

    CFunctionDecl(name, parameters, returnType, Some(body))
  }

  def getProductEquals(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl =
    getProductEquals(getProductRecordDecl(tpe, ctx), ctx)

  def getProductEquals(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    ctx.nameToRecordEquals.getOrElseUpdate(recordDecl.name, buildProductEquals(recordDecl, ctx))
  }

  def buildProductEquals(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", CRecordType(recordDecl))
    val paramRight = CParmVarDecl("right", CRecordType(recordDecl))
    val parameters = List(paramLeft, paramRight)

    val returnType = CBoolType

    val comparisons: List[CStmt] = recordDecl.fields.map { f =>
      val cond = f.declaredType.unqualType match {
        case CRecordType(rd) =>
          val eq = getProductEquals(rd, ctx)
          CNotExpr(
            CCallExpr(
              CDeclRefExpr(eq),
              List(
                CMemberExpr(CDeclRefExpr(paramLeft), f),
                CMemberExpr(CDeclRefExpr(paramRight), f)
              )
            )
          )
        case _ =>
          CNotEqualsExpr(
            CMemberExpr(CDeclRefExpr(paramLeft), f),
            CMemberExpr(CDeclRefExpr(paramRight), f)
          )
      }

      CIfStmt(
        cond,
        CReturnStmt(Some(CFalseLiteral))
      )
    }
    val body = CCompoundStmt(comparisons.appended(CReturnStmt(Some(CTrueLiteral))))

    CFunctionDecl(name, parameters, returnType, Some(body))
  }

  def getProductRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(typeName(tpe), compileProductTypeToCRecordDecl(tpe, ctx))
  }

  def compileProductTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    import quotes.reflect.*

    val classSymbol = tpe.classSymbol.get

    val fields = classSymbol.caseFields.collect {
      case symbol if symbol.isValDef =>
        CFieldDecl(symbol.name.strip(), compileTypeRepr(tpe.memberType(symbol), ctx))
    }

    CRecordDecl(typeName(tpe), fields)
  }
}
