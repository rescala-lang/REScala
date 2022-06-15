package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.{CAssignmentExpr, CEqualsExpr, CGreaterThanExpr, CLessThanExpr}
import clangast.expr.unaryop.{CIncExpr, CNotExpr}
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CForStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.{StdArgH, StdLibH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.base.CompileApply.varArgs
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

object CompileArray extends SelectPC with ApplyPC with TypePC with StringPC {
  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*
  
      {
        case Select(arr, "length") =>
          CMemberExpr(
            cascade.dispatch(_.compileTermToCExpr)(arr),
            lengthField
          )
      }
    }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC](compileSelectImpl)
  
  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*
    
      {
        case apply @ this.arrayApply(args) =>
          val elems = args.map(cascade.dispatch(_.compileTermToCExpr))

          CCallExpr(getArrayCreator(apply.tpe).ref, CIntegerLiteral(elems.length) :: elems)
        case Apply(Select(arr, "apply"), List(idx)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          arrayIndexAccess(arr, idx)
        case Apply(Select(arr, "update"), List(idx, v)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          CAssignmentExpr(
            arrayIndexAccess(arr, idx),
            cascade.dispatch(_.compileTermToCExpr)(v)
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  def compileEqualsImpl(using Quotes)(using RecordDeclTC, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Array[?]] =>
          CParenExpr(CEqualsExpr(
            CMemberExpr(leftExpr, dataField),
            CMemberExpr(rightExpr, dataField)
          ))
      }
    }

  override def compileEquals(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
      ensureCtx[RecordDeclTC](compileEqualsImpl)

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*
  
      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          getArrayRecordDecl(tpe).getTypeForDecl
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] => cascade.dispatch(_.classTypeName)(tpe)
      }
    }

  def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
          CCallExpr(getArrayPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private val dataField: String = "data"
  private val lengthField: String = "length"

  private def getArrayRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), compileArrayTypeToCRecordDecl(tpe))
  }

  private def compileArrayTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(elemType)) = tpe

    val dataFieldDecl = CFieldDecl(dataField, CPointerType(cascade.dispatch(_.compileTypeRepr)(elemType)))
    val lengthFieldDecl = CFieldDecl(lengthField, CIntegerType)

    CRecordDecl("Array_" + cascade.dispatch(_.typeName)(elemType), List(dataFieldDecl, lengthFieldDecl))
  }

  private def arrayApply(using Quotes): PartialFunction[quotes.reflect.Apply, List[quotes.reflect.Term]] = apply => {
    import quotes.reflect.*

    apply match {
      case Apply(Select(Ident("Array"), "apply"), varArgs(args)) => args
      case Apply(TypeApply(Select(Ident("Array"), "apply"), _), varArgs(args)) => args
    }
  }

  val CREATE = "CREATE"
  val PRINT = "PRINT"

  private def getArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE, buildArrayCreator(getArrayRecordDecl(tpe)))
  }

  private def buildArrayCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "create_" + recordDecl.name

    val lengthParam = CParmVarDecl("length", CIntegerType)

    val CFieldDecl(_, CQualType(CPointerType(elemType), _)) = recordDecl.getField(dataField)

    val arrDecl =
      CVarDecl(
        "arr",
        recordDecl.getTypeForDecl,
        Some(CDesignatedInitExpr(List(
          dataField -> CCastExpr(
            CCallExpr(
              StdLibH.calloc.ref,
              List(
                lengthParam.ref,
                CSizeofExpr(Left(elemType.unqualType))
              )
            ),
            CPointerType(elemType)
          ),
          lengthField -> lengthParam.ref
        )))
      )

    val argpDecl = CVarDecl("argp", StdArgH.va_list.getTypeForDecl)

    val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop =
      CForStmt(
        Some(iDecl),
        Some(CLessThanExpr(iDecl.ref, lengthParam.ref)),
        Some(CIncExpr(iDecl.ref)),
        CCompoundStmt(List(
          CAssignmentExpr(
            CArraySubscriptExpr(
              CMemberExpr(arrDecl.ref, dataField),
              iDecl.ref
            ),
            CCallExpr(
              StdArgH.va_arg.ref,
              List(argpDecl.ref, CTypeArgExpr(elemType.unqualType))
            )
          )
        ))
      )

    val body = CCompoundStmt(List(
      arrDecl,
      argpDecl,
      CCallExpr(StdArgH.va_start.ref, List(argpDecl.ref, lengthParam.ref)),
      loop,
      CCallExpr(StdArgH.va_end.ref, List(argpDecl.ref)),
      CReturnStmt(Some(arrDecl.ref))
    ))

    CFunctionDecl(name, List(lengthParam), recordDecl.getTypeForDecl, Some(body), variadic = true)
  }

  private def arrayIndexAccess(using Quotes)(arr: quotes.reflect.Term, idx: quotes.reflect.Term)(using ctx: RecordDeclTC, cascade: CompilerCascade): CArraySubscriptExpr = {
    CArraySubscriptExpr(
      CMemberExpr(
        cascade.dispatch(_.compileTermToCExpr)(arr),
        dataField
      ),
      cascade.dispatch(_.compileTermToCExpr)(idx)
    )
  }

  private def getArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildArrayPrinter(tpe))
  }

  private def buildArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getArrayRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen

    val name = "print_" + recordDecl.name

    val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

    val iter = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop = CForStmt(
      Some(iter),
      Some(CLessThanExpr(iter.ref, CMemberExpr(arrayParam.ref, lengthField))),
      Some(CIncExpr(iter.ref)),
      CCompoundStmt(List(
        CIfStmt(CGreaterThanExpr(iter.ref, 0.lit), CompileString.printf(", ")),
        cascade.dispatch(_.compilePrint)(
          CArraySubscriptExpr(
            CMemberExpr(arrayParam.ref, dataField),
            iter.ref
          ),
          elemType
        )
      ))
    )

    val body = CCompoundStmt(List(
      CompileString.printf("["),
      loop,
      CompileString.printf("]")
    ))

    CFunctionDecl(
      name,
      List(arrayParam),
      CVoidType,
      Some(body),
    )
  }
}
