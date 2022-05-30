package macros

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAssignmentExpr, CLessThanExpr}
import clangast.expr.unaryop.CIncExpr
import clangast.stmt.*
import clangast.stubs.{StdArgH, StdLibH}
import clangast.types.*

import CompileTerm.compileTermToCExpr
import CompileType.*
import CompileApply.varArgs

import scala.quoted.*

object CompileArray {
  def compileApply(using Quotes)(ctx: TranslationContext): PartialFunction[quotes.reflect.Apply, CExpr] = apply => {
    import quotes.reflect.*

    apply match {
      case this.arrayApply(args) =>
        val elems = args.map(compileTermToCExpr(_, ctx))

        val creator = getArrayCreator(apply.tpe, ctx)

        CCallExpr(CDeclRefExpr(creator), CIntegerLiteral(elems.length) :: elems)
    }
  }

  def arrayApply(using Quotes): PartialFunction[quotes.reflect.Apply, List[quotes.reflect.Term]] = apply => {
    import quotes.reflect.*

    apply match {
      case Apply(Select(Ident("Array"), "apply"), varArgs(args)) => args
      case Apply(TypeApply(Select(Ident("Array"), "apply"), _), varArgs(args)) => args
    }
  }

  def getArrayRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(typeName(tpe), compileArrayTypeToCRecordDecl(tpe, ctx))
  }

  def compileArrayTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(elemType)) = tpe

    val dataField = CFieldDecl("data", CPointerType(compileTypeRepr(elemType, ctx)))
    val lengthField = CFieldDecl("length", CIntegerType)

    CRecordDecl("Array_" + typeName(elemType), List(dataField, lengthField))
  }

  def getArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl = {
    ctx.nameToRecordCreator.getOrElseUpdate(typeName(tpe), buildArrayCreator(getArrayRecordDecl(tpe, ctx), ctx))
  }

  def buildArrayCreator(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    val name = "create_" + recordDecl.name

    val lengthParam = CParmVarDecl("length", CIntegerType)

    val returnType = CRecordType(recordDecl)

    ctx.includes.add(StdLibH.include)
    ctx.includes.add(StdArgH.include)

    val dataField = recordDecl.fields.find(_.name.equals("data")).get

    val elemType = dataField match {
      case CFieldDecl(_, CQualType(CPointerType(elemType), _)) => elemType
      case _ => throw new MatchError(recordDecl)
    }

    val arrDecl =
      CVarDecl(
        "arr",
        CRecordType(recordDecl),
        Some(CDesignatedInitExpr(List(
          "data" -> CCastExpr(
            CCallExpr(
              CDeclRefExpr(StdLibH.calloc),
              List(
                CDeclRefExpr(lengthParam),
                CSizeofExpr(Left(elemType.unqualType))
              )
            ),
            CPointerType(elemType)
          ),
          "length" -> CDeclRefExpr(lengthParam)
        )))
      )

    val argpDecl = CVarDecl("argp", CTypedefType(StdArgH.va_list))

    val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop =
      CForStmt(
        Some(iDecl),
        Some(CLessThanExpr(CDeclRefExpr(iDecl), CDeclRefExpr(lengthParam))),
        Some(CIncExpr(CDeclRefExpr(iDecl))),
        CCompoundStmt(List(
          CAssignmentExpr(
            CArraySubscriptExpr(
              CMemberExpr(CDeclRefExpr(arrDecl), dataField),
              CDeclRefExpr(iDecl)
            ),
            CCallExpr(
              CDeclRefExpr(StdArgH.va_arg),
              List(CDeclRefExpr(argpDecl), CTypeArgExpr(elemType.unqualType))
            )
          )
        ))
      )

    val body = CCompoundStmt(List(
      arrDecl,
      argpDecl,
      CCallExpr(CDeclRefExpr(StdArgH.va_start), List(CDeclRefExpr(argpDecl), CDeclRefExpr(lengthParam))),
      loop,
      CCallExpr(CDeclRefExpr(StdArgH.va_end), List(CDeclRefExpr(argpDecl))),
      CReturnStmt(Some(CDeclRefExpr(arrDecl)))
    ))

    val decl = CFunctionDecl(name, List(lengthParam), returnType, Some(body), variadic = true)

    println(decl.textgen)

    decl
  }
}
