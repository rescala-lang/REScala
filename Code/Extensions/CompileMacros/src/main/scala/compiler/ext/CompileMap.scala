package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.{CAddressExpr, CDerefExpr, CIncExpr}
import clangast.expr.*
import clangast.stmt.*
import clangast.stubs.{HashmapH, StdLibH, StringH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileApply.varArgs
import compiler.base.CompileDataStructure.{retain, release}
import compiler.base.CompileType.typeArgs
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*
import scala.collection.mutable

object CompileMap extends SelectPC with ApplyPC with MatchPC with TypePC with DataStructurePC with StringPC {
  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case apply @ Apply(TypeApply(Select(Select(Ident("mutable"), "Map"), "apply"), _), List(Typed(Repeated(Nil, _), _))) =>
          CCallExpr(getMapCreator(apply.tpe).ref, List())
        case Apply(Select(map, "get"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          getForKey(map, key)
        case Apply(Select(map, "apply"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CMemberExpr(
            getForKey(map, key),
            CompileOption.valField
          )
        case Apply(Select(map, "contains"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CMemberExpr(
            getForKey(map, key),
            CompileOption.definedField
          )
        case apply @ Apply(TypeApply(Select(map, "getOrElse"), _), List(key, default)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CCallExpr(
            CompileOption.getGetOrElse(TypeRepr.of[Option].appliedTo(apply.tpe)).ref,
            List(
              getForKey(map, key),
              cascade.dispatch(_.compileTermToCExpr)(default)
            )
          )
        case Apply(Select(map, "update"), List(key, value)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CCallExpr(
            getMapUpdate(map.tpe).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(map),
              cascade.dispatch(_.compileTermToCExpr)(key),
              cascade.dispatch(_.compileTermToCExpr)(value)
            )
          )
        case Apply(Select(map, "remove"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CCallExpr(
            getMapRemove(map.tpe).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(map),
              cascade.dispatch(_.compileTermToCExpr)(key)
            )
          )
        case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(map)), List()) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CCallExpr(
            getMapDeepCopy(map.tpe).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(map))
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          getRecordDecl(tpe).getTypeForDecl
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
  PartialFunction[quotes.reflect.TypeRepr, String] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] => cascade.dispatch(_.classTypeName)(tpe)
    }
  }

  override def compileTypeToCRecordDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] && validKeyType(tpe) =>
          val dataFieldDecl = CFieldDecl(dataField, HashmapH.map_t)
          val refCountFieldDecl = CFieldDecl(refCountField, CPointerType(CIntegerType))

          val recName = cascade.dispatch(_.typeName)(tpe)

          CRecordDecl(recName, List(dataFieldDecl, refCountFieldDecl))
      }
    }

  override def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] => true
      }
    }

  private def compileFreeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          val typeArgs(List(_, valueType)) = tpe

          val releaseValues = cascade.dispatch(_.usesRefCount)(valueType)

          val freeThis: List[CStmt] = List(
            CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountField))),
            CCallExpr(HashmapH.hashmap_free.ref, List(CMemberExpr(expr, dataField)))
          )

          if releaseValues then
            val iterate = CCallExpr(
              HashmapH.hashmap_iterate.ref,
              List(
                CMemberExpr(expr, dataField),
                CAddressExpr(getMapReleaseIterator(tpe).ref),
                CNullLiteral
              )
            )

            CCompoundStmt(iterate :: freeThis)
          else CCompoundStmt(freeThis)
      }
    }

  override def compileFree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC](compileFreeImpl)

  private def compileDeepCopyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Map[?, ?]] =>
          getMapDeepCopy(tpe)
      }
    }

  override def compileDeepCopy(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeepCopyImpl)

  private def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CCallExpr(getMapPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private val dataField: String = "data"

  val CREATE = "CREATE"
  val GET = "GET"
  val UPDATE = "UPDATE"
  val REMOVE = "REMOVE"
  val RELEASE_ITERATOR = "RELEASE_ITERATOR"
  val DEEP_COPY_ITERATOR = "DEEP_COPY_ITERATOR"
  val PRINT = "PRINT"
  val PRINT_ITERATOR = "PRINT_ITERATOR"

  private def validKeyType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*

    val typeArgs(List(keyType, _)) = tpe
    keyType <:< TypeRepr.of[String]
  }

  private def getMapCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE, buildMapCreator(tpe))
  }

  private def buildMapCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)

    val name = "create_" + recordDecl.name

    val mapDecl =
      CVarDecl(
        "map",
        recordDecl.getTypeForDecl,
        Some(CDesignatedInitExpr(List(
          dataField -> CCallExpr(HashmapH.hashmap_new.ref, List()),
          allocRefCount(tpe).get
        )))
      )

    val body = CCompoundStmt(List(
      mapDecl,
      CReturnStmt(Some(mapDecl.ref))
    ))

    CFunctionDecl(name, List(), recordDecl.getTypeForDecl, Some(body))
  }

  private def getForKey(using Quotes)(map: quotes.reflect.Term, key: quotes.reflect.Term)(using ctx: RecordDeclTC, cascade: CompilerCascade): CExpr =
    CCallExpr(
      getMapGet(map.tpe).ref,
      List(
        cascade.dispatch(_.compileTermToCExpr)(map),
        cascade.dispatch(_.compileTermToCExpr)(key)
      )
    )

  private def getMapGet(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> GET, buildMapGet(tpe))
  }

  private def buildMapGet(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(keyType, valueType)) = tpe.widen

    val valueOptionType = TypeRepr.of[Option].appliedTo(valueType)

    val returnType = cascade.dispatch(_.compileTypeRepr)(valueOptionType)

    val keyCType = cascade.dispatch(_.compileTypeRepr)(keyType)
    val valueCType = cascade.dispatch(_.compileTypeRepr)(valueType)

    val name = "get_" + recordDecl.name

    val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
    val keyParam = CParmVarDecl("key", keyCType)

    val valueHandleDecl = CVarDecl(
      "valueHandle",
      CPointerType(CPointerType(valueCType)),
      Some(CCastExpr(
        CCallExpr(
          StdLibH.malloc.ref,
          List(CSizeofExpr(Left(CPointerType(valueCType))))
        ),
        CPointerType(CPointerType(valueCType))
      ))
    )

    val statusDecl = CVarDecl(
      "status",
      CIntegerType,
      Some(CCallExpr(
        HashmapH.hashmap_get.ref,
        List(
          CMemberExpr(mapParam.ref, dataField),
          keyParam.ref,
          CCastExpr(valueHandleDecl.ref, CPointerType(HashmapH.any_t))
        )
      ))
    )

    val valueDecl = CVarDecl("value", valueCType, Some(CDerefExpr(CDerefExpr(valueHandleDecl.ref))))

    val freeValueHandle = CCallExpr(StdLibH.free.ref, List(valueHandleDecl.ref))

    val res = CIfStmt(
      CEqualsExpr(statusDecl.ref, HashmapH.MAP_OK.ref),
      CCompoundStmt(List(
        valueDecl,
        freeValueHandle,
        CReturnStmt(Some(CCallExpr(
          CompileOption.getSomeCreator(valueOptionType).ref,
          List(valueDecl.ref)
        )))
      )),
      Some(CCompoundStmt(List(
        freeValueHandle,
        CReturnStmt(Some(CCallExpr(
          CompileOption.getNoneCreator(valueOptionType).ref,
          List()
        )))
      )))
    )

    val body = CCompoundStmt(List(
      valueHandleDecl,
      statusDecl,
      res
    ))

    CFunctionDecl(name, List(mapParam, keyParam), returnType, Some(body))
  }

  private def getMapUpdate(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> UPDATE, buildMapUpdate(tpe))
  }

  private def buildMapUpdate(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(keyType, valueType)) = tpe.widen

    val keyCType = cascade.dispatch(_.compileTypeRepr)(keyType)
    val valueCType = cascade.dispatch(_.compileTypeRepr)(valueType)

    val name = "update_" + recordDecl.name

    val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
    val keyParam = CParmVarDecl("key", keyCType)
    val valueParam = CParmVarDecl("value", valueCType)

    val keyStringDecl = CVarDecl(
      "keyString",
      CPointerType(CCharType),
      Some(CCastExpr(
        CCallExpr(
          StdLibH.malloc.ref,
          List(CProdExpr(
            CCallExpr(StringH.strlen.ref, List(keyParam.ref)),
            CSizeofExpr(Left(CCharType))
          ))
        ),
        CPointerType(CCharType)
      ))
    )

    val keyCopy = CCallExpr(
      StringH.strcpy.ref,
      List(keyStringDecl.ref, keyParam.ref)
    )

    val valuePointerDecl = CVarDecl(
      "valuePointer",
      CPointerType(valueCType),
      Some(CCastExpr(
        CCallExpr(
          StdLibH.malloc.ref,
          List(CSizeofExpr(Left(valueCType)))
        ),
        CPointerType(valueCType)
      ))
    )

    val valuePointerAssign = CAssignmentExpr(CDerefExpr(valuePointerDecl.ref), retain(valueParam.ref, valueType))

    val put = CCallExpr(
      HashmapH.hashmap_put.ref,
      List(
        CMemberExpr(mapParam.ref, dataField),
        keyStringDecl.ref,
        CCastExpr(valuePointerDecl.ref, HashmapH.any_t)
      )
    )

    val body = CCompoundStmt(List(
      keyStringDecl,
      keyCopy,
      valuePointerDecl,
      valuePointerAssign,
      put
    ))

    CFunctionDecl(name, List(mapParam, keyParam, valueParam), CVoidType, Some(body))
  }

  private def getMapRemove(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> REMOVE, buildMapRemove(tpe))
  }

  private def buildMapRemove(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(keyType, _)) = tpe.widen

    val keyCType = cascade.dispatch(_.compileTypeRepr)(keyType)

    val name = "remove_" + recordDecl.name

    val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
    val keyParam = CParmVarDecl("key", keyCType)

    val body = CCompoundStmt(List(
      CCallExpr(
        HashmapH.hashmap_remove.ref,
        List(CMemberExpr(mapParam.ref, dataField), keyParam.ref)
      )
    ))

    CFunctionDecl(name, List(mapParam, keyParam), CVoidType, Some(body))
  }

  private def getMapReleaseIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> RELEASE_ITERATOR, buildMapReleaseIterator(tpe))
  }

  private def buildMapReleaseIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(_, valueType)) = tpe.widen

    val valueCType = cascade.dispatch(_.compileTypeRepr)(valueType)

    val name = "releaseIterator_" + recordDecl.name

    val itemParam = CParmVarDecl("item", HashmapH.any_t)
    val keyParam = CParmVarDecl("key", CPointerType(CCharType))
    val dataParam = CParmVarDecl("data", HashmapH.any_t)

    val valueDecl = CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

    val releaseCall = release(CDerefExpr(valueDecl.ref), valueType, CFalseLiteral).get

    val body = CCompoundStmt(List(
      valueDecl,
      releaseCall,
      CReturnStmt(Some(HashmapH.MAP_OK.ref))
    ))

    CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
  }

  private def getMapDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DEEP_COPY, buildMapDeepCopy(tpe))
  }

  private def buildMapDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)

    val name = "deepCopy_" + recordDecl.name

    val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)

    val copyDecl = CVarDecl(
      "copy",
      recordDecl.getTypeForDecl,
      Some(retain(CCallExpr(getMapCreator(tpe).ref, List()), tpe))
    )

    val iterate = CCallExpr(
      HashmapH.hashmap_iterate.ref,
      List(
        CMemberExpr(mapParam.ref, dataField),
        CAddressExpr(getMapDeepCopyIterator(tpe).ref),
        CCastExpr(CMemberExpr(copyDecl.ref, dataField), HashmapH.any_t)
      )
    )

    val body = CCompoundStmt(List(
      copyDecl,
      iterate,
      CReturnStmt(Some(copyDecl.ref))
    ))

    CFunctionDecl(name, List(mapParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getMapDeepCopyIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DEEP_COPY_ITERATOR, buildMapDeepCopyIterator(tpe))
  }

  private def buildMapDeepCopyIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(_, valueType)) = tpe.widen

    val valueCType = cascade.dispatch(_.compileTypeRepr)(valueType)

    val name = "deepCopyIterator_" + recordDecl.name

    val itemParam = CParmVarDecl("item", HashmapH.any_t)
    val keyParam = CParmVarDecl("key", CPointerType(CCharType))
    val dataParam = CParmVarDecl("data", HashmapH.any_t)

    val mapDecl = CVarDecl("map", HashmapH.map_t, Some(CCastExpr(itemParam.ref, HashmapH.map_t)))
    val valueDecl = CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

    val keyCopyDecl = CVarDecl(
      "keyCopy",
      CPointerType(CCharType),
      Some(CCastExpr(
        CCallExpr(
          StdLibH.malloc.ref,
          List(CProdExpr(CCallExpr(StringH.strlen.ref, List(keyParam.ref)), CSizeofExpr(Left(CPointerType(CCharType)))))
        ),
        CPointerType(CCharType)
      ))
    )
    val copyKey = CCallExpr(StringH.strcpy.ref, List(keyCopyDecl.ref, keyParam.ref))

    val valuePointerCopyDecl = CVarDecl(
      "valuePointerCopy",
      CPointerType(valueCType),
      Some(CCastExpr(
        CCallExpr(
          StdLibH.malloc.ref,
          List(CSizeofExpr(Left(valueCType)))
        ),
        CPointerType(valueCType)
      ))
    )

    val valuePointerAssign = CAssignmentExpr(
      CDerefExpr(valuePointerCopyDecl.ref),
      retain(CompileDataStructure.deepCopy(CDerefExpr(valueDecl.ref), valueType), valueType)
    )

    val put = CCallExpr(
      HashmapH.hashmap_put.ref,
      List(
        mapDecl.ref,
        keyCopyDecl.ref,
        CCastExpr(valuePointerCopyDecl.ref, HashmapH.any_t)
      )
    )

    val body = CCompoundStmt(List(
      mapDecl,
      valueDecl,
      valuePointerCopyDecl,
      valuePointerAssign,
      keyCopyDecl,
      copyKey,
      put,
      CReturnStmt(Some(HashmapH.MAP_OK.ref))
    ))

    CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
  }

  private def getMapPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildMapPrinter(tpe))
  }

  private def buildMapPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)

    val name = "print_" + recordDecl.name

    val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)

    val counterDecl = CVarDecl("counter", CIntegerType, Some(0.lit))

    val iterate = CCallExpr(
      HashmapH.hashmap_iterate.ref,
      List(
        CMemberExpr(mapParam.ref, dataField),
        CAddressExpr(getMapPrintIterator(tpe).ref),
        CCastExpr(CAddressExpr(counterDecl.ref), HashmapH.any_t)
      )
    )

    val body = CCompoundStmt(List(
      CompileString.printf("Map("),
      counterDecl,
      iterate,
      CompileString.printf(")")
    ))

    CFunctionDecl(name, List(mapParam), CVoidType, Some(body))
  }

  private def getMapPrintIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT_ITERATOR, buildMapPrintIterator(tpe))
  }

  private def buildMapPrintIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(_, valueType)) = tpe.widen

    val valueCType = cascade.dispatch(_.compileTypeRepr)(valueType)

    val name = "printIterator_" + recordDecl.name

    val itemParam = CParmVarDecl("item", HashmapH.any_t)
    val keyParam = CParmVarDecl("key", CPointerType(CCharType))
    val dataParam = CParmVarDecl("data", HashmapH.any_t)

    val counterDecl = CVarDecl("counterPointer", CPointerType(CIntegerType), Some(CCastExpr(itemParam.ref, CPointerType(CIntegerType))))
    val valueDecl = CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

    val printComma = CIfStmt(CGreaterThanExpr(CDerefExpr(counterDecl.ref), 0.lit), CompileString.printf(", "))
    val printKey = CompileString.printf("\\\"%s\\\" -> ", keyParam.ref)
    val printValue = cascade.dispatch(_.compilePrint)(CDerefExpr(valueDecl.ref), valueType)

    val incCounter = CIncExpr(CParenExpr(CDerefExpr(counterDecl.ref)))

    val body = CCompoundStmt(List(
      counterDecl,
      valueDecl,
      printComma,
      printKey,
      printValue,
      incCounter,
      CReturnStmt(Some(HashmapH.MAP_OK.ref))
    ))

    CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
  }
}