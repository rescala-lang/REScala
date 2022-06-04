package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.{CAssignmentExpr, CLessThanExpr}
import clangast.expr.unaryop.CIncExpr
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CForStmt, CReturnStmt}
import clangast.stubs.{StdArgH, StdLibH}
import clangast.types.*
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}
import compiler.base.CompileType.{typeArgs, typeName}
import compiler.base.CompileApply.varArgs

import scala.quoted.*

object CompileArray extends PartialCompiler {
  override def compileApply(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
  PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*
    
      {
        case apply @ this.arrayApply(args) =>
          val elems = args.map(cascade.compileTermToCExpr)

          val creator = getArrayCreator(apply.tpe)

          CCallExpr(CDeclRefExpr(creator), CIntegerLiteral(elems.length) :: elems)
        case Apply(Select(arr, "apply"), List(idx)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          arrayIndexAccess(arr, idx)
        case Apply(Select(arr, "update"), List(idx, v)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          CAssignmentExpr(
            arrayIndexAccess(arr, idx),
            cascade.compileTermToCExpr(v)
          )
      }
    }

  override def compileSelect(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*
    
      {
        case Select(arr, "length") =>
          val recordDecl = getArrayRecordDecl(arr.tpe)
          val lengthField = recordDecl.fields.find(_.name.equals("length")).get

          CMemberExpr(
            cascade.compileTermToCExpr(arr),
            lengthField
          )
      }
    }

  override def compileTypeRepr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*
  
      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          CRecordType(getArrayRecordDecl(tpe))
      }
    }

  private def arrayIndexAccess(using Quotes)(arr: quotes.reflect.Term, idx: quotes.reflect.Term)(using ctx: TranslationContext, cascade: CompilerCascade): CArraySubscriptExpr = {
    val recordDecl = getArrayRecordDecl(arr.tpe)
    val dataField = recordDecl.fields.find(_.name.equals("data")).get

    CArraySubscriptExpr(
      CMemberExpr(
        cascade.compileTermToCExpr(arr),
        dataField
      ),
      cascade.compileTermToCExpr(idx)
    )
  }

  private def arrayApply(using Quotes): PartialFunction[quotes.reflect.Apply, List[quotes.reflect.Term]] = apply => {
    import quotes.reflect.*

    apply match {
      case Apply(Select(Ident("Array"), "apply"), varArgs(args)) => args
      case Apply(TypeApply(Select(Ident("Array"), "apply"), _), varArgs(args)) => args
    }
  }

  private def getArrayRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(typeName(tpe), compileArrayTypeToCRecordDecl(tpe))
  }

  private def compileArrayTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(elemType)) = tpe

    val dataField = CFieldDecl("data", CPointerType(cascade.compileTypeRepr(elemType)))
    val lengthField = CFieldDecl("length", CIntegerType)

    CRecordDecl("Array_" + typeName(elemType), List(dataField, lengthField))
  }

  private def getArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CFunctionDecl = {
    ctx.nameToRecordCreator.getOrElseUpdate(typeName(tpe), buildArrayCreator(getArrayRecordDecl(tpe)))
  }

  private def buildArrayCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
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
