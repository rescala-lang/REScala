package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.{CAssignmentExpr, CEqualsExpr, CGreaterThanExpr}
import clangast.expr.unaryop.{CAddressExpr, CDerefExpr, CIncExpr}
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.{CJSONH, HashmapH, StdLibH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileDataStructure.{release, retain}
import compiler.base.CompileType.typeArgs
import compiler.context.{RecordDeclTC, TranslationContext}
import compiler.ext.CompileSerialization.{deserialize, serialize}

import scala.quoted.*
import scala.collection.mutable

object CompileSet extends ApplyPC with TypePC with DataStructurePC with StringPC with SerializationPC {
  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case apply @ Apply(TypeApply(Select(Select(Ident("mutable"), "Set"), "apply"), _), List(Typed(Repeated(Nil, _), _))) =>
          CCallExpr(CompileMap.getMapCreator(mapType(apply.tpe)).ref, List())
        case Apply(Select(set, "contains"), List(elem)) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(
            CompileMap.getMapContains(mapType(set.tpe)).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(set),
              cascade.dispatch(_.compileTermToCExpr)(elem)
            )
          )
        case Apply(Select(set, "add"), List(elem)) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(
            getSetAdd(set.tpe).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(set),
              cascade.dispatch(_.compileTermToCExpr)(elem)
            )
          )
        case Apply(Select(set, "remove"), List(key)) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(
            CompileMap.getMapRemove(mapType(set.tpe)).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(set),
              cascade.dispatch(_.compileTermToCExpr)(key)
            )
          )
        case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(set)), List()) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(
            CompileMap.getMapDeepCopy(mapType(set.tpe)).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(set))
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  private def compileEqualsImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(
            CompileMap.getMapEquals(mapType(leftType)).ref,
            List(leftExpr, rightExpr)
          )
      }
    }

  override def compileEquals(using Quotes)(using TranslationContext, CompilerCascade):
  PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
    ensureCtx[RecordDeclTC](compileEqualsImpl)

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          getRecordDecl(mapType(tpe)).getTypeForDecl
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          cascade.dispatch(_.typeName)(mapType(tpe))
      }
    }

  private def defaultValueImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(CompileMap.getMapCreator(mapType(tpe)).ref, List())
      }
    }

  override def defaultValue(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC](defaultValueImpl)

  override def compileTypeToCRecordDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          cascade.dispatch(_.compileTypeToCRecordDecl)(mapType(tpe))
      }
    }

  override def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] => true
      }
    }

  override def compileFree(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          cascade.dispatch(_.compileFree)(expr, mapType(tpe))
      }
    }

  override def compileDeepCopy(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          cascade.dispatch(_.compileDeepCopy)(mapType(tpe))
      }
    }

  private def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(getSetPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private def compileSerializeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          getSetSerialize(tpe)
      }
    }

  override def compileSerialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileSerializeImpl)

  private def compileDeserializeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
          getSetDeserialize(tpe)
      }
    }

  override def compileDeserialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeserializeImpl)

  private def mapType(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*

    val typeArgs(List(elemType)) = tpe.widen
    TypeRepr.of[mutable.Map].appliedTo(List(elemType, elemType))
  }

  private val ADD = "ADD"
  private val PRINT_SET = "PRINT_SET"
  private val PRINT_SET_ITERATOR = "PRINT_SET_ITERATOR"
  private val SERIALIZE_ITERATOR = "SERIALIZE_ITERATOR"

  private def getSetAdd(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> ADD, buildSetAdd(tpe))
  }

  private def buildSetAdd(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(mapType(tpe))
    val typeArgs(List(elemType)) = tpe.widen

    val elemCType = cascade.dispatch(_.compileTypeRepr)(elemType)

    val name = "add_" + recordDecl.name

    val setParam = CParmVarDecl("set", recordDecl.getTypeForDecl)
    val elemParam = CParmVarDecl("elem", elemCType)

    CCallExpr(
      CompileMap.getMapUpdate(mapType(tpe)).ref,
      List(setParam.ref, elemParam.ref, elemParam.ref)
    )

    val body = CCompoundStmt(List(
      CCallExpr(
        CompileMap.getMapUpdate(mapType(tpe)).ref,
        List(setParam.ref, elemParam.ref, elemParam.ref)
      )
    ))

    CFunctionDecl(name, List(setParam, elemParam), CVoidType, Some(body))
  }

  private def getSetPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT_SET, buildSetPrinter(tpe))
  }

  private def buildSetPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(mapType(tpe))

    val name = "printSet_" + recordDecl.name

    val setParam = CParmVarDecl("set", recordDecl.getTypeForDecl)

    val counterDecl = CVarDecl("counter", CIntegerType, Some(0.lit))

    val iterate = CCallExpr(
      HashmapH.hashmap_iterate.ref,
      List(
        CMemberExpr(setParam.ref, CompileMap.dataField),
        CAddressExpr(getSetPrintIterator(tpe).ref),
        CCastExpr(CAddressExpr(counterDecl.ref), HashmapH.any_t)
      )
    )

    val body = CCompoundStmt(List(
      CompileString.printf("Set("),
      counterDecl,
      iterate,
      CompileString.printf(")")
    ))

    CFunctionDecl(name, List(setParam), CVoidType, Some(body))
  }

  private def getSetPrintIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT_SET_ITERATOR, buildSetPrintIterator(tpe))
  }

  private def buildSetPrintIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(mapType(tpe))
    val typeArgs(List(elemType)) = tpe.widen

    val elemCType = cascade.dispatch(_.compileTypeRepr)(elemType)

    val name = "printSetIterator_" + recordDecl.name

    val itemParam = CParmVarDecl("item", HashmapH.any_t)
    val keyParam = CParmVarDecl("key", CPointerType(CCharType))
    val dataParam = CParmVarDecl("data", HashmapH.any_t)

    val counterDecl = CVarDecl("counterPointer", CPointerType(CIntegerType), Some(CCastExpr(itemParam.ref, CPointerType(CIntegerType))))
    val elemDecl = CVarDecl("elemPointer", CPointerType(elemCType), Some(CCastExpr(dataParam.ref, CPointerType(elemCType))))

    val printComma = CIfStmt(CGreaterThanExpr(CDerefExpr(counterDecl.ref), 0.lit), CompileString.printf(", "))
    val printElem = cascade.dispatch(_.compilePrint)(CDerefExpr(elemDecl.ref), elemType)

    val incCounter = CIncExpr(CParenExpr(CDerefExpr(counterDecl.ref)))

    val body = CCompoundStmt(List(
      counterDecl,
      elemDecl,
      printComma,
      printElem,
      incCounter,
      CReturnStmt(Some(HashmapH.MAP_OK.ref))
    ))

    CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
  }

  private def getSetSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> SERIALIZE, buildSetSerialize(tpe))
  }

  private def buildSetSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(mapType(tpe))

    val name = "serializeSet_" + recordDecl.name

    val setParam = CParmVarDecl("set", recordDecl.getTypeForDecl)

    val jsonDecl = CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

    val iterate = CCallExpr(
      HashmapH.hashmap_iterate.ref,
      List(
        CMemberExpr(setParam.ref, CompileMap.dataField),
        CAddressExpr(getSetSerializeIterator(tpe).ref),
        CCastExpr(jsonDecl.ref, HashmapH.any_t)
      )
    )

    val body = CCompoundStmt(List(
      jsonDecl,
      iterate,
      CReturnStmt(Some(jsonDecl.ref))
    ))

    CFunctionDecl(name, List(setParam), CPointerType(CJSONH.cJSON), Some(body))
  }

  private def getSetSerializeIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> SERIALIZE_ITERATOR, buildSetSerializeIterator(tpe))
  }

  private def buildSetSerializeIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(mapType(tpe))

    val name = "serializeSetIterator_" + recordDecl.name

    val itemParam = CParmVarDecl("item", HashmapH.any_t)
    val keyParam = CParmVarDecl("key", CPointerType(CCharType))
    val dataParam = CParmVarDecl("data", HashmapH.any_t)

    val jsonDecl = CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCastExpr(itemParam.ref, CPointerType(CJSONH.cJSON))))

    val body = CCompoundStmt(List(
      jsonDecl,
      CCallExpr(
        CJSONH.cJSON_AddItemToArray.ref,
        List(jsonDecl.ref, CCallExpr(CJSONH.cJSON_Parse.ref, List(keyParam.ref)))
      ),
      CReturnStmt(Some(HashmapH.MAP_OK.ref))
    ))

    CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
  }

  private def getSetDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DESERIALIZE, buildSetDeserialize(tpe))
  }

  private def buildSetDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(mapType(tpe))
    val typeArgs(List(valueType)) = tpe.widen

    val name = "deserializeSet_" + recordDecl.name

    val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

    val setDecl = CVarDecl("set", recordDecl.getTypeForDecl, Some(CCallExpr(CompileMap.getMapCreator(tpe).ref, List())))

    val elemDecl = CVarDecl("elem", CPointerType(CJSONH.cJSON))
    val loop = CJSONH.cJSON_ArrayForEach(elemDecl.ref, jsonParam.ref, CCompoundStmt(List(
      CCallExpr(getSetAdd(tpe).ref, List(setDecl.ref, deserialize(elemDecl.ref, valueType)))
    )))

    val body = CCompoundStmt(List(
      setDecl,
      elemDecl,
      loop,
      CReturnStmt(Some(setDecl.ref))
    ))

    CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
  }
}
