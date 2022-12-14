package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.{CAddressExpr, CDerefExpr, CIncExpr}
import clangast.expr.*
import clangast.stmt.*
import clangast.stubs.{CJSONH, HashmapH, StdBoolH, StdLibH, StringH}
import clangast.types.*
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.DataStructureFragment.{release, retain}
import compiler.base.TypeFragment.typeArgs
import compiler.context.{RecordDeclTC, TranslationContext}
import compiler.ext.SerializationFragment.{deserialize, serialize}

import scala.quoted.*
import scala.collection.mutable

object MapFragment extends ApplyIFFragment with TypeIFFragment with DataStructureIFFragment with StringIFFragment
    with SerializationIFFragment {
  override def compileApply(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case apply @ Apply(
            TypeApply(Select(Select(Ident("mutable"), "Map"), "apply"), _),
            List(Typed(Repeated(Nil, _), _))
          ) =>
        CCallExpr(getMapCreator(apply.tpe).ref, List())
      case Apply(Select(map, "get"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapGet(map.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(map),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key)
          )
        )
      case Apply(Select(map, "apply"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapApply(map.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(map),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key)
          )
        )
      case Apply(Select(map, "contains"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapContains(map.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(map),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key)
          )
        )
      case Apply(TypeApply(Select(map, "getOrElse"), _), List(key, default))
          if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapGetOrElse(map.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(map),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key),
            dispatch[TermIFFragment](_.compileTermToCExpr)(default)
          )
        )
      case Apply(Select(map, "update"), List(key, value)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapUpdate(map.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(map),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key),
            dispatch[TermIFFragment](_.compileTermToCExpr)(value)
          )
        )
      case Apply(Select(map, "remove"), List(key)) if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapRemove(map.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(map),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key)
          )
        )
      case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(map)), List())
          if map.tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(
          getMapDeepCopy(map.tpe).ref,
          List(dispatch[TermIFFragment](_.compileTermToCExpr)(map))
        )
    }
  }

  override def compileEquals(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[mutable.Map[?, ?]] =>
          CCallExpr(
            getMapEquals(leftType).ref,
            List(leftExpr, rightExpr)
          )
      }
    }

  override def compileTypeRepr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        getRecordDecl(tpe).getTypeForDecl
    }
  }

  override def typeName(using Quotes)(using FragmentedCompiler)(using
      ctx: TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] => dispatch[TypeIFFragment](_.classTypeName)(tpe)
    }
  }

  override def defaultValue(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(getMapCreator(tpe).ref, List())
    }
  }

  override def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      ctx: TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] && validKeyType(tpe) =>
        val dataFieldDecl     = CFieldDecl(dataField, HashmapH.map_t)
        val refCountFieldDecl = CFieldDecl(refCountFieldName, CPointerType(CIntegerType))

        val recName = dispatch[TypeIFFragment](_.typeName)(tpe)

        CRecordDecl(recName, List(dataFieldDecl, refCountFieldDecl))
    }
  }

  override def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] => true
    }
  }

  override def compileFree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        val typeArgs(List(_, valueType)) = tpe.widen: @unchecked

        val releaseValues = dispatch[DataStructureIFFragment](_.usesRefCount)(valueType)

        val freeThis: List[CStmt] = List(
          CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountFieldName))),
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

  override def compileDeepCopy(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        getMapDeepCopy(tpe)
    }
  }

  override def compilePrint(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        CCallExpr(getMapPrinter(tpe).ref, List(expr))
    }
  }

  override def compileSerialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        getMapSerialize(tpe)
    }
  }

  override def compileDeserialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Map[?, ?]] =>
        getMapDeserialize(tpe)
    }
  }

  val dataField: String = "data"

  private val CREATE             = "CREATE"
  private val GET                = "GET"
  private val APPLY              = "APPLY"
  private val CONTAINS           = "CONTAINS"
  private val GET_OR_ELSE        = "GET_OR_ELSE"
  private val UPDATE             = "UPDATE"
  private val REMOVE             = "REMOVE"
  private val RELEASE_ITERATOR   = "RELEASE_ITERATOR"
  private val EQUALS             = "EQUALS"
  private val EQUALS_ITERATOR    = "EQUALS_ITERATOR"
  private val DEEP_COPY_ITERATOR = "DEEP_COPY_ITERATOR"
  private val PRINT              = "PRINT"
  private val PRINT_ITERATOR     = "PRINT_ITERATOR"
  private val SERIALIZE_ITERATOR = "SERIALIZE_ITERATOR"

  private def validKeyType(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      TranslationContext
  ): Boolean = {
    import quotes.reflect.*

    val typeArgs(List(keyType, _)) = tpe.widen: @unchecked
    dispatch[SerializationIFFragment](_.serializationRetainsEquality)(keyType)
  }

  def getMapCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> CREATE, {
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
    )
  }

  private def getMapGet(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> GET, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val valueOptionType = TypeRepr.of[Option].appliedTo(valueType)

        val returnType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueOptionType)

        val keyCType   = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)
        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "get_" + recordDecl.name

        val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
        val keyParam = CParmVarDecl("key", keyCType)

        val keyJsonDecl = CVarDecl("keyJSON", CPointerType(CJSONH.cJSON), Some(serialize(keyParam.ref, keyType)))
        val keyStringDecl = CVarDecl(
          "keyString",
          CPointerType(CCharType),
          Some(CCallExpr(CJSONH.cJSON_Print.ref, List(keyJsonDecl.ref)))
        )

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
              keyStringDecl.ref,
              CCastExpr(valueHandleDecl.ref, CPointerType(HashmapH.any_t))
            )
          ))
        )

        val freeKeyString = CCallExpr(StdLibH.free.ref, List(keyStringDecl.ref))

        val valueDecl = CVarDecl("value", valueCType, Some(CDerefExpr(CDerefExpr(valueHandleDecl.ref))))

        val freeValueHandle = CCallExpr(StdLibH.free.ref, List(valueHandleDecl.ref))

        val res = CIfStmt(
          CEqualsExpr(statusDecl.ref, HashmapH.MAP_OK.ref),
          CCompoundStmt(List(
            valueDecl,
            freeValueHandle,
            CReturnStmt(Some(CCallExpr(
              OptionFragment.getSomeCreator(valueOptionType).ref,
              List(valueDecl.ref)
            )))
          )),
          Some(CCompoundStmt(List(
            freeValueHandle,
            CReturnStmt(Some(CCallExpr(
              OptionFragment.getNoneCreator(valueOptionType).ref,
              List()
            )))
          )))
        )

        val body = CCompoundStmt(List(
          keyJsonDecl,
          keyStringDecl,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJsonDecl.ref)),
          valueHandleDecl,
          statusDecl,
          freeKeyString,
          res
        ))

        CFunctionDecl(name, List(mapParam, keyParam), returnType, Some(body))
      }
    )
  }

  private def getMapApply(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> APPLY, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val keyCType   = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)
        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "apply_" + recordDecl.name

        val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
        val keyParam = CParmVarDecl("key", keyCType)

        val keyJsonDecl = CVarDecl("keyJSON", CPointerType(CJSONH.cJSON), Some(serialize(keyParam.ref, keyType)))
        val keyStringDecl = CVarDecl(
          "keyString",
          CPointerType(CCharType),
          Some(CCallExpr(CJSONH.cJSON_Print.ref, List(keyJsonDecl.ref)))
        )

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

        val getCall = CCallExpr(
          HashmapH.hashmap_get.ref,
          List(
            CMemberExpr(mapParam.ref, dataField),
            keyStringDecl.ref,
            CCastExpr(valueHandleDecl.ref, CPointerType(HashmapH.any_t))
          )
        )

        val freeKeyString = CCallExpr(StdLibH.free.ref, List(keyStringDecl.ref))

        val valueDecl = CVarDecl("value", valueCType, Some(CDerefExpr(CDerefExpr(valueHandleDecl.ref))))

        val freeValueHandle = CCallExpr(StdLibH.free.ref, List(valueHandleDecl.ref))

        val body = CCompoundStmt(List(
          keyJsonDecl,
          keyStringDecl,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJsonDecl.ref)),
          valueHandleDecl,
          getCall,
          freeKeyString,
          valueDecl,
          freeValueHandle,
          CReturnStmt(Some(valueDecl.ref))
        ))

        CFunctionDecl(name, List(mapParam, keyParam), valueCType, Some(body))
      }
    )
  }

  def getMapContains(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> CONTAINS, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val keyCType   = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)
        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "contains_" + recordDecl.name

        val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
        val keyParam = CParmVarDecl("key", keyCType)

        val keyJsonDecl = CVarDecl("keyJSON", CPointerType(CJSONH.cJSON), Some(serialize(keyParam.ref, keyType)))
        val keyStringDecl = CVarDecl(
          "keyString",
          CPointerType(CCharType),
          Some(CCallExpr(CJSONH.cJSON_Print.ref, List(keyJsonDecl.ref)))
        )

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
              keyStringDecl.ref,
              CCastExpr(valueHandleDecl.ref, CPointerType(HashmapH.any_t))
            )
          ))
        )

        val freeKeyString = CCallExpr(StdLibH.free.ref, List(keyStringDecl.ref))

        val freeValueHandle = CCallExpr(StdLibH.free.ref, List(valueHandleDecl.ref))

        val body = CCompoundStmt(List(
          keyJsonDecl,
          keyStringDecl,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJsonDecl.ref)),
          valueHandleDecl,
          statusDecl,
          freeKeyString,
          freeValueHandle,
          CReturnStmt(Some(CEqualsExpr(statusDecl.ref, HashmapH.MAP_OK.ref)))
        ))

        CFunctionDecl(name, List(mapParam, keyParam), StdBoolH.bool, Some(body))
      }
    )
  }

  private def getMapGetOrElse(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> GET_OR_ELSE, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val keyCType   = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)
        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "getOrElse_" + recordDecl.name

        val mapParam     = CParmVarDecl("map", recordDecl.getTypeForDecl)
        val keyParam     = CParmVarDecl("key", keyCType)
        val defaultParam = CParmVarDecl("default", valueCType)

        val keyJsonDecl = CVarDecl("keyJSON", CPointerType(CJSONH.cJSON), Some(serialize(keyParam.ref, keyType)))
        val keyStringDecl = CVarDecl(
          "keyString",
          CPointerType(CCharType),
          Some(CCallExpr(CJSONH.cJSON_Print.ref, List(keyJsonDecl.ref)))
        )

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
              keyStringDecl.ref,
              CCastExpr(valueHandleDecl.ref, CPointerType(HashmapH.any_t))
            )
          ))
        )

        val freeKeyString = CCallExpr(StdLibH.free.ref, List(keyStringDecl.ref))

        val valueDecl = CVarDecl("value", valueCType, Some(CDerefExpr(CDerefExpr(valueHandleDecl.ref))))

        val freeValueHandle = CCallExpr(StdLibH.free.ref, List(valueHandleDecl.ref))

        val res = CIfStmt(
          CEqualsExpr(statusDecl.ref, HashmapH.MAP_OK.ref),
          CCompoundStmt(List(
            valueDecl,
            freeValueHandle,
            CReturnStmt(Some(valueDecl.ref))
          )),
          Some(CCompoundStmt(List(
            freeValueHandle,
            CReturnStmt(Some(defaultParam.ref))
          )))
        )

        val body = CCompoundStmt(List(
          keyJsonDecl,
          keyStringDecl,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJsonDecl.ref)),
          valueHandleDecl,
          statusDecl,
          freeKeyString,
          res
        ))

        CFunctionDecl(name, List(mapParam, keyParam), valueCType, Some(body))
      }
    )
  }

  def getMapUpdate(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> UPDATE, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val keyCType   = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)
        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "update_" + recordDecl.name

        val mapParam   = CParmVarDecl("map", recordDecl.getTypeForDecl)
        val keyParam   = CParmVarDecl("key", keyCType)
        val valueParam = CParmVarDecl("value", valueCType)

        val keyJsonDecl = CVarDecl("keyJSON", CPointerType(CJSONH.cJSON), Some(serialize(keyParam.ref, keyType)))
        val keyStringDecl = CVarDecl(
          "keyString",
          CPointerType(CCharType),
          Some(CCallExpr(CJSONH.cJSON_Print.ref, List(keyJsonDecl.ref)))
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
          keyJsonDecl,
          keyStringDecl,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJsonDecl.ref)),
          valuePointerDecl,
          valuePointerAssign,
          put
        ))

        CFunctionDecl(name, List(mapParam, keyParam, valueParam), CVoidType, Some(body))
      }
    )
  }

  def getMapRemove(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> REMOVE, {
        import quotes.reflect.*

        val recordDecl                 = getRecordDecl(tpe)
        val typeArgs(List(keyType, _)) = tpe.widen: @unchecked

        val keyCType = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)

        val name = "remove_" + recordDecl.name

        val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)
        val keyParam = CParmVarDecl("key", keyCType)

        val keyJsonDecl = CVarDecl("keyJSON", CPointerType(CJSONH.cJSON), Some(serialize(keyParam.ref, keyType)))
        val keyStringDecl = CVarDecl(
          "keyString",
          CPointerType(CCharType),
          Some(CCallExpr(CJSONH.cJSON_Print.ref, List(keyJsonDecl.ref)))
        )

        val callRemove = CCallExpr(
          HashmapH.hashmap_remove.ref,
          List(CMemberExpr(mapParam.ref, dataField), keyStringDecl.ref)
        )

        val freeKeyString = CCallExpr(StdLibH.free.ref, List(keyStringDecl.ref))

        val body = CCompoundStmt(List(
          keyJsonDecl,
          keyStringDecl,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJsonDecl.ref)),
          callRemove,
          freeKeyString
        ))

        CFunctionDecl(name, List(mapParam, keyParam), CVoidType, Some(body))
      }
    )
  }

  private def getMapReleaseIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> RELEASE_ITERATOR, {
        import quotes.reflect.*

        val recordDecl                   = getRecordDecl(tpe)
        val typeArgs(List(_, valueType)) = tpe.widen: @unchecked

        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "releaseIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val valueDecl =
          CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

        val releaseCall = release(CDerefExpr(valueDecl.ref), valueType, CFalseLiteral).get

        val body = CCompoundStmt(List(
          valueDecl,
          releaseCall,
          CReturnStmt(Some(HashmapH.MAP_OK.ref))
        ))

        CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
      }
    )
  }

  def getMapEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      getRecordDecl(tpe).name -> EQUALS, {
        val recordDecl = getRecordDecl(tpe)

        val name = "equals_" + recordDecl.name

        val leftParam  = CParmVarDecl("left", recordDecl.getTypeForDecl)
        val rightParam = CParmVarDecl("right", recordDecl.getTypeForDecl)

        def isSubSet(a: CExpr, b: CExpr): CExpr =
          CEqualsExpr(
            CCallExpr(
              HashmapH.hashmap_iterate.ref,
              List(
                CMemberExpr(a, dataField),
                CAddressExpr(getMapEqualsIterator(tpe).ref),
                CCastExpr(CMemberExpr(b, dataField), HashmapH.any_t)
              )
            ),
            HashmapH.MAP_OK.ref
          )

        val body = CCompoundStmt(List(
          CReturnStmt(Some(CAndExpr(
            isSubSet(leftParam.ref, rightParam.ref),
            isSubSet(rightParam.ref, leftParam.ref)
          )))
        ))

        CFunctionDecl(name, List(leftParam, rightParam), StdBoolH.bool, Some(body))
      }
    )
  }

  private def getMapEqualsIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> EQUALS_ITERATOR, {
        import quotes.reflect.*

        val recordDecl                   = getRecordDecl(tpe)
        val typeArgs(List(_, valueType)) = tpe.widen: @unchecked

        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "equalsIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val valueDecl = CVarDecl(
          "value",
          valueCType,
          Some(CDerefExpr(CParenExpr(CCastExpr(dataParam.ref, CPointerType(valueCType)))))
        )
        val otherDecl = CVarDecl("other", HashmapH.map_t, Some(CCastExpr(itemParam.ref, HashmapH.map_t)))
        val otherValueDecl = CVarDecl(
          "otherValue",
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
              otherDecl.ref,
              keyParam.ref,
              CCastExpr(otherValueDecl.ref, CPointerType(HashmapH.any_t))
            )
          ))
        )

        val retDecl = CVarDecl("ret", CIntegerType)
        val retAssign = CIfStmt(
          CAndExpr(
            CEqualsExpr(statusDecl.ref, HashmapH.MAP_OK.ref),
            CEqualsExpr(
              CCallExpr(
                StringH.strcmp.ref,
                List(valueDecl.ref, CDerefExpr(CDerefExpr(otherValueDecl.ref)))
              ),
              0.lit
            )
          ),
          CAssignmentExpr(retDecl.ref, HashmapH.MAP_OK.ref),
          Some(CAssignmentExpr(retDecl.ref, HashmapH.MAP_MISSING.ref))
        )

        val freeHandle = CCallExpr(StdLibH.free.ref, List(otherValueDecl.ref))

        val body = CCompoundStmt(List(
          valueDecl,
          otherDecl,
          otherValueDecl,
          CEmptyStmt,
          statusDecl,
          retDecl,
          retAssign,
          CEmptyStmt,
          freeHandle,
          CReturnStmt(Some(retDecl.ref))
        ))

        CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
      }
    )
  }

  def getMapDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DEEP_COPY, {
        import quotes.reflect.*

        val recordDecl = getRecordDecl(tpe)

        val name = "deepCopy_" + recordDecl.name

        val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)

        val copyDecl = CVarDecl(
          "copy",
          recordDecl.getTypeForDecl,
          Some(CCallExpr(getMapCreator(tpe).ref, List()))
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
    )
  }

  private def getMapDeepCopyIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DEEP_COPY_ITERATOR, {
        import quotes.reflect.*

        val recordDecl                   = getRecordDecl(tpe)
        val typeArgs(List(_, valueType)) = tpe.widen: @unchecked

        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "deepCopyIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val mapDecl = CVarDecl("map", HashmapH.map_t, Some(CCastExpr(itemParam.ref, HashmapH.map_t)))
        val valueDecl =
          CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

        val keyCopyDecl = CVarDecl(
          "keyCopy",
          CPointerType(CCharType),
          Some(CCastExpr(
            CCallExpr(
              StdLibH.malloc.ref,
              List(CProdExpr(
                CCallExpr(StringH.strlen.ref, List(keyParam.ref)),
                CSizeofExpr(Left(CPointerType(CCharType)))
              ))
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
          retain(DataStructureFragment.deepCopy(CDerefExpr(valueDecl.ref), valueType), valueType)
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
    )
  }

  private def getMapPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT, {
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
          StringFragment.printf("Map("),
          counterDecl,
          iterate,
          StringFragment.printf(")")
        ))

        CFunctionDecl(name, List(mapParam), CVoidType, Some(body))
      }
    )
  }

  private def getMapPrintIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT_ITERATOR, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val keyCType   = dispatch[TypeIFFragment](_.compileTypeRepr)(keyType)
        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "printIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val counterDecl = CVarDecl(
          "counterPointer",
          CPointerType(CIntegerType),
          Some(CCastExpr(itemParam.ref, CPointerType(CIntegerType)))
        )
        val valueDecl =
          CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

        val printComma = CIfStmt(CGreaterThanExpr(CDerefExpr(counterDecl.ref), 0.lit), StringFragment.printf(", "))
        val keyJson =
          CVarDecl("keyJson", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_Parse.ref, List(keyParam.ref))))
        val parsedKey = CVarDecl("parsedKey", keyCType, Some(deserialize(keyJson.ref, keyType)))
        val printKey  = dispatch[StringIFFragment](_.compilePrint)(parsedKey.ref, keyType)
        val releaseKey = CCompoundStmt(
          CCallExpr(CJSONH.cJSON_Delete.ref, List(keyJson.ref)) :: release(parsedKey.ref, keyType, CFalseLiteral).toList
        )
        val printArrow = StringFragment.printf(" -> ")
        val printValue = dispatch[StringIFFragment](_.compilePrint)(CDerefExpr(valueDecl.ref), valueType)

        val incCounter = CIncExpr(CParenExpr(CDerefExpr(counterDecl.ref)))

        val body = CCompoundStmt(List(
          counterDecl,
          valueDecl,
          printComma,
          keyJson,
          parsedKey,
          printKey,
          releaseKey,
          printArrow,
          printValue,
          incCounter,
          CReturnStmt(Some(HashmapH.MAP_OK.ref))
        ))

        CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
      }
    )
  }

  private def getMapSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE, {
        import quotes.reflect.*

        val recordDecl = getRecordDecl(tpe)

        val name = "serialize_" + recordDecl.name

        val mapParam = CParmVarDecl("map", recordDecl.getTypeForDecl)

        val jsonDecl =
          CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

        val iterate = CCallExpr(
          HashmapH.hashmap_iterate.ref,
          List(
            CMemberExpr(mapParam.ref, dataField),
            CAddressExpr(getMapSerializeIterator(tpe).ref),
            CCastExpr(jsonDecl.ref, HashmapH.any_t)
          )
        )

        val body = CCompoundStmt(List(
          jsonDecl,
          iterate,
          CReturnStmt(Some(jsonDecl.ref))
        ))

        CFunctionDecl(name, List(mapParam), CPointerType(CJSONH.cJSON), Some(body))
      }
    )
  }

  private def getMapSerializeIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE_ITERATOR, {
        import quotes.reflect.*

        val recordDecl                   = getRecordDecl(tpe)
        val typeArgs(List(_, valueType)) = tpe.widen: @unchecked

        val valueCType = dispatch[TypeIFFragment](_.compileTypeRepr)(valueType)

        val name = "serializeIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val jsonDecl =
          CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCastExpr(itemParam.ref, CPointerType(CJSONH.cJSON))))
        val valueDecl =
          CVarDecl("valuePointer", CPointerType(valueCType), Some(CCastExpr(dataParam.ref, CPointerType(valueCType))))

        val pairDecl =
          CVarDecl("pair", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

        val body = CCompoundStmt(List(
          jsonDecl,
          valueDecl,
          pairDecl,
          CCallExpr(
            CJSONH.cJSON_AddItemToArray.ref,
            List(pairDecl.ref, CCallExpr(CJSONH.cJSON_Parse.ref, List(keyParam.ref)))
          ),
          CCallExpr(
            CJSONH.cJSON_AddItemToArray.ref,
            List(pairDecl.ref, serialize(CDerefExpr(valueDecl.ref), valueType))
          ),
          CCallExpr(
            CJSONH.cJSON_AddItemToArray.ref,
            List(jsonDecl.ref, pairDecl.ref)
          ),
          CReturnStmt(Some(HashmapH.MAP_OK.ref))
        ))

        CFunctionDecl(name, List(itemParam, keyParam, dataParam), CIntegerType, Some(body))
      }
    )
  }

  private def getMapDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DESERIALIZE, {
        import quotes.reflect.*

        val recordDecl                         = getRecordDecl(tpe)
        val typeArgs(List(keyType, valueType)) = tpe.widen: @unchecked

        val name = "deserialize_" + recordDecl.name

        val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

        val mapDecl = CVarDecl("map", recordDecl.getTypeForDecl, Some(CCallExpr(getMapCreator(tpe).ref, List())))

        val pairDecl = CVarDecl("pair", CPointerType(CJSONH.cJSON))
        val loop = CJSONH.cJSON_ArrayForEach(
          pairDecl.ref,
          jsonParam.ref,
          CCompoundStmt(List(
            CCallExpr(
              getMapUpdate(tpe).ref,
              List(
                mapDecl.ref,
                deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(pairDecl.ref, 0.lit)), keyType),
                deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(pairDecl.ref, 1.lit)), valueType)
              )
            )
          ))
        )

        val body = CCompoundStmt(List(
          mapDecl,
          pairDecl,
          loop,
          CReturnStmt(Some(mapDecl.ref))
        ))

        CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }
}
