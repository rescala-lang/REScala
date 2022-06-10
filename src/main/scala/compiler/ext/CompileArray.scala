package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.{CAssignmentExpr, CGreaterThanExpr, CLessThanExpr}
import clangast.expr.unaryop.CIncExpr
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
            getLengthField(getArrayRecordDecl(arr.tpe))
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

          val creator = getArrayCreator(apply.tpe)

          CCallExpr(CDeclRefExpr(creator), CIntegerLiteral(elems.length) :: elems)
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

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*
  
      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          CRecordType(getArrayRecordDecl(tpe))
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
          CCallExpr(CDeclRefExpr(getArrayPrinter(tpe)), List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private def getLengthField(recordDecl: CRecordDecl): CFieldDecl =
    recordDecl.fields.find(_.name.equals("length")).get

  private def getDataField(recordDecl: CRecordDecl): CFieldDecl =
    recordDecl.fields.find(_.name.equals("data")).get

  private def getArrayRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), compileArrayTypeToCRecordDecl(tpe))
  }

  private def compileArrayTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(elemType)) = tpe

    val dataField = CFieldDecl("data", CPointerType(cascade.dispatch(_.compileTypeRepr)(elemType)))
    val lengthField = CFieldDecl("length", CIntegerType)

    CRecordDecl("Array_" + cascade.dispatch(_.typeName)(elemType), List(dataField, lengthField))
  }

  private def arrayApply(using Quotes): PartialFunction[quotes.reflect.Apply, List[quotes.reflect.Term]] = apply => {
    import quotes.reflect.*

    apply match {
      case Apply(Select(Ident("Array"), "apply"), varArgs(args)) => args
      case Apply(TypeApply(Select(Ident("Array"), "apply"), _), varArgs(args)) => args
    }
  }

  private def getArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.nameToRecordCreator.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), buildArrayCreator(getArrayRecordDecl(tpe)))
  }

  private def buildArrayCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "create_" + recordDecl.name

    val lengthParam = CParmVarDecl("length", CIntegerType)

    val returnType = CRecordType(recordDecl)

    val dataField = getDataField(recordDecl)

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

    CFunctionDecl(name, List(lengthParam), returnType, Some(body), variadic = true)
  }

  private def arrayIndexAccess(using Quotes)(arr: quotes.reflect.Term, idx: quotes.reflect.Term)(using ctx: RecordDeclTC, cascade: CompilerCascade): CArraySubscriptExpr = {
    CArraySubscriptExpr(
      CMemberExpr(
        cascade.dispatch(_.compileTermToCExpr)(arr),
        getDataField(getArrayRecordDecl(arr.tpe))
      ),
      cascade.dispatch(_.compileTermToCExpr)(idx)
    )
  }

  private def getArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.nameToRecordPrinter.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), buildArrayPrinter(tpe))
  }

  private def buildArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getArrayRecordDecl(tpe)
    val lengthField = getLengthField(recordDecl)
    val dataField = getDataField(recordDecl)
    val AppliedType(_, List(elemType)) = tpe.widen

    val name = "print_" + recordDecl.name

    val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

    val iter = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop = CForStmt(
      Some(iter),
      Some(CLessThanExpr(CDeclRefExpr(iter), CMemberExpr(CDeclRefExpr(arrayParam), lengthField))),
      Some(CIncExpr(CDeclRefExpr(iter))),
      CCompoundStmt(List(
        CIfStmt(CGreaterThanExpr(CDeclRefExpr(iter), 0.lit), CompileString.printf(", ")),
        cascade.dispatch(_.compilePrint)(
          CArraySubscriptExpr(
            CMemberExpr(CDeclRefExpr(arrayParam), dataField),
            CDeclRefExpr(iter)
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
