package macros

import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.CNotEqualsExpr
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.*
import clangast.types.*

import scala.quoted.*

object CompileType {
  def isNumberType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*

    tpe <:< TypeRepr.of[Byte] ||
      tpe <:< TypeRepr.of[Short] ||
      tpe <:< TypeRepr.of[Char] ||
      tpe <:< TypeRepr.of[Int] ||
      tpe <:< TypeRepr.of[Long] ||
      tpe <:< TypeRepr.of[Float] ||
      tpe <:< TypeRepr.of[Double]
  }

  def compileTypeRepr(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CType = {
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Boolean] then CBoolType
    else if tpe =:= TypeRepr.of[Byte] then CCharType
    else if tpe =:= TypeRepr.of[Char] then CCharType
    else if tpe =:= TypeRepr.of[Short] then CShortType
    else if tpe =:= TypeRepr.of[Int] then CIntegerType
    else if tpe =:= TypeRepr.of[Long] then CLongType
    else if tpe =:= TypeRepr.of[Float] then CFloatType
    else if tpe =:= TypeRepr.of[Double] then CDoubleType
    else if tpe =:= TypeRepr.of[Unit] then CVoidType
    else if tpe <:< TypeRepr.of[Product] then CRecordType(getRecordDecl(tpe, ctx))
    else throw new MatchError(tpe.show(using Printer.TypeReprStructure))
  }

  def getRecordCreator(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl = {
    import quotes.reflect.*

    val recName = recordName(tpe)

    ctx.nameToRecordCreator.get(recName) match {
      case Some(decl) => decl
      case None =>
        val recordDecl = getRecordDecl(tpe, ctx)
        val decl = buildRecordCreator(recordDecl)
        ctx.nameToRecordCreator.put(recName, decl)
        decl
    }
  }

  def buildRecordCreator(recordDecl: CRecordDecl): CFunctionDecl = {
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

  def getRecordEquals(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl =
    getRecordEquals(getRecordDecl(tpe, ctx), ctx)

  def getRecordEquals(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    ctx.nameToRecordEquals.get(recordDecl.name) match {
      case Some(decl) => decl
      case None =>
        val decl = buildRecordEquals(recordDecl, ctx)
        ctx.nameToRecordEquals.put(recordDecl.name, decl)
        decl
    }
  }

  def buildRecordEquals(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", CRecordType(recordDecl))
    val paramRight = CParmVarDecl("right", CRecordType(recordDecl))
    val parameters = List(paramLeft, paramRight)

    val returnType = CBoolType

    val comparisons: List[CStmt] = recordDecl.fields.map { f =>
      val cond = f.declaredType.unqualType match {
        case CRecordType(rd) =>
          val eq = getRecordEquals(rd, ctx)
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

  def getRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    import quotes.reflect.*

    val recName = recordName(tpe)

    ctx.nameToRecordDecl.get(recName) match {
      case Some(decl) => decl
      case None =>
        val decl = compileTypeReprToCRecordDecl(tpe, ctx)
        ctx.nameToRecordDecl.put(recName, decl)
        decl
    }
  }

  def recordName(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect.*

    val symbolName = tpe.classSymbol.get.name

    tpe.widen match {
      case AppliedType(_, typeArgs) =>
        val typeArgNames = typeArgs.map { t =>
          if t <:< TypeRepr.of[Product] then recordName(t)
          else t.typeSymbol.name
        }
        symbolName + "_" + typeArgNames.mkString("_")
      case _ => symbolName
    }
  }

  def compileTypeReprToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    import quotes.reflect.*

    val classSymbol = tpe.classSymbol.get

    val fields = classSymbol.caseFields.collect {
      case symbol if symbol.isValDef =>
        CFieldDecl(symbol.name.strip(), compileTypeRepr(tpe.memberType(symbol), ctx))
    }

    CRecordDecl(recordName(tpe), fields)
  }
}
