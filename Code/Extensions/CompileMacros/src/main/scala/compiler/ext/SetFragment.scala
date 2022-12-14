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
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.DataStructureFragment.{release, retain}
import compiler.base.TypeFragment.typeArgs
import compiler.context.{RecordDeclTC, TranslationContext}
import compiler.ext.SerializationFragment.{deserialize, serialize}

import scala.quoted.*
import scala.collection.mutable

object SetFragment extends ApplyIFFragment with TypeIFFragment with DataStructureIFFragment with StringIFFragment
    with SerializationIFFragment {
  override def compileApply(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case apply @ Apply(
            TypeApply(Select(Select(Ident("mutable"), "Set"), "apply"), _),
            List(Typed(Repeated(Nil, _), _))
          ) =>
        CCallExpr(MapFragment.getMapCreator(mapType(apply.tpe)).ref, List())
      case Apply(Select(set, "contains"), List(elem)) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
        CCallExpr(
          MapFragment.getMapContains(mapType(set.tpe)).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(set),
            dispatch[TermIFFragment](_.compileTermToCExpr)(elem)
          )
        )
      case Apply(Select(set, "add"), List(elem)) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
        CCallExpr(
          getSetAdd(set.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(set),
            dispatch[TermIFFragment](_.compileTermToCExpr)(elem)
          )
        )
      case Apply(Select(set, "remove"), List(key)) if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
        CCallExpr(
          MapFragment.getMapRemove(mapType(set.tpe)).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(set),
            dispatch[TermIFFragment](_.compileTermToCExpr)(key)
          )
        )
      case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(set)), List())
          if set.tpe <:< TypeRepr.of[mutable.Set[?]] =>
        CCallExpr(
          MapFragment.getMapDeepCopy(mapType(set.tpe)).ref,
          List(dispatch[TermIFFragment](_.compileTermToCExpr)(set))
        )
    }
  }

  override def compileEquals(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[mutable.Set[?]] =>
          CCallExpr(
            MapFragment.getMapEquals(mapType(leftType)).ref,
            List(leftExpr, rightExpr)
          )
      }
    }

  override def compileTypeRepr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        getRecordDecl(mapType(tpe)).getTypeForDecl
    }
  }

  override def typeName(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        dispatch[TypeIFFragment](_.typeName)(mapType(tpe))
    }
  }

  override def defaultValue(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        CCallExpr(MapFragment.getMapCreator(mapType(tpe)).ref, List())
    }
  }

  override def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        dispatch[DataStructureIFFragment](_.compileTypeToCRecordDecl)(mapType(tpe))
    }
  }

  override def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] => true
    }
  }

  override def compileFree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        dispatch[DataStructureIFFragment](_.compileFree)(expr, mapType(tpe))
    }
  }

  override def compileDeepCopy(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        dispatch[DataStructureIFFragment](_.compileDeepCopy)(mapType(tpe))
    }
  }

  override def compilePrint(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        CCallExpr(getSetPrinter(tpe).ref, List(expr))
    }
  }

  override def compileSerialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        getSetSerialize(tpe)
    }
  }

  override def compileDeserialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[mutable.Set[?]] =>
        getSetDeserialize(tpe)
    }
  }

  private def mapType(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*

    val typeArgs(List(elemType)) = tpe.widen: @unchecked
    TypeRepr.of[mutable.Map].appliedTo(List(elemType, elemType))
  }

  private val ADD                = "ADD"
  private val PRINT_SET          = "PRINT_SET"
  private val PRINT_SET_ITERATOR = "PRINT_SET_ITERATOR"
  private val SERIALIZE_ITERATOR = "SERIALIZE_ITERATOR"

  private def getSetAdd(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> ADD, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(mapType(tpe))
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val elemCType = dispatch[TypeIFFragment](_.compileTypeRepr)(elemType)

        val name = "add_" + recordDecl.name

        val setParam  = CParmVarDecl("set", recordDecl.getTypeForDecl)
        val elemParam = CParmVarDecl("elem", elemCType)

        CCallExpr(
          MapFragment.getMapUpdate(mapType(tpe)).ref,
          List(setParam.ref, elemParam.ref, elemParam.ref)
        )

        val body = CCompoundStmt(List(
          CCallExpr(
            MapFragment.getMapUpdate(mapType(tpe)).ref,
            List(setParam.ref, elemParam.ref, elemParam.ref)
          )
        ))

        CFunctionDecl(name, List(setParam, elemParam), CVoidType, Some(body))
      }
    )
  }

  private def getSetPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT_SET, {
        import quotes.reflect.*

        val recordDecl = getRecordDecl(mapType(tpe))

        val name = "printSet_" + recordDecl.name

        val setParam = CParmVarDecl("set", recordDecl.getTypeForDecl)

        val counterDecl = CVarDecl("counter", CIntegerType, Some(0.lit))

        val iterate = CCallExpr(
          HashmapH.hashmap_iterate.ref,
          List(
            CMemberExpr(setParam.ref, MapFragment.dataField),
            CAddressExpr(getSetPrintIterator(tpe).ref),
            CCastExpr(CAddressExpr(counterDecl.ref), HashmapH.any_t)
          )
        )

        val body = CCompoundStmt(List(
          StringFragment.printf("Set("),
          counterDecl,
          iterate,
          StringFragment.printf(")")
        ))

        CFunctionDecl(name, List(setParam), CVoidType, Some(body))
      }
    )
  }

  private def getSetPrintIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT_SET_ITERATOR, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(mapType(tpe))
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val elemCType = dispatch[TypeIFFragment](_.compileTypeRepr)(elemType)

        val name = "printSetIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val counterDecl = CVarDecl(
          "counterPointer",
          CPointerType(CIntegerType),
          Some(CCastExpr(itemParam.ref, CPointerType(CIntegerType)))
        )
        val elemDecl =
          CVarDecl("elemPointer", CPointerType(elemCType), Some(CCastExpr(dataParam.ref, CPointerType(elemCType))))

        val printComma = CIfStmt(CGreaterThanExpr(CDerefExpr(counterDecl.ref), 0.lit), StringFragment.printf(", "))
        val printElem  = dispatch[StringIFFragment](_.compilePrint)(CDerefExpr(elemDecl.ref), elemType)

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
    )
  }

  private def getSetSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE, {
        import quotes.reflect.*

        val recordDecl = getRecordDecl(mapType(tpe))

        val name = "serializeSet_" + recordDecl.name

        val setParam = CParmVarDecl("set", recordDecl.getTypeForDecl)

        val jsonDecl =
          CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

        val iterate = CCallExpr(
          HashmapH.hashmap_iterate.ref,
          List(
            CMemberExpr(setParam.ref, MapFragment.dataField),
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
    )
  }

  private def getSetSerializeIterator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE_ITERATOR, {
        import quotes.reflect.*

        val recordDecl = getRecordDecl(mapType(tpe))

        val name = "serializeSetIterator_" + recordDecl.name

        val itemParam = CParmVarDecl("item", HashmapH.any_t)
        val keyParam  = CParmVarDecl("key", CPointerType(CCharType))
        val dataParam = CParmVarDecl("data", HashmapH.any_t)

        val jsonDecl =
          CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCastExpr(itemParam.ref, CPointerType(CJSONH.cJSON))))

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
    )
  }

  private def getSetDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DESERIALIZE, {
        import quotes.reflect.*

        val recordDecl                = getRecordDecl(mapType(tpe))
        val typeArgs(List(valueType)) = tpe.widen: @unchecked

        val name = "deserializeSet_" + recordDecl.name

        val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

        val setDecl =
          CVarDecl("set", recordDecl.getTypeForDecl, Some(CCallExpr(MapFragment.getMapCreator(tpe).ref, List())))

        val elemDecl = CVarDecl("elem", CPointerType(CJSONH.cJSON))
        val loop = CJSONH.cJSON_ArrayForEach(
          elemDecl.ref,
          jsonParam.ref,
          CCompoundStmt(List(
            CCallExpr(getSetAdd(tpe).ref, List(setDecl.ref, deserialize(elemDecl.ref, valueType)))
          ))
        )

        val body = CCompoundStmt(List(
          setDecl,
          elemDecl,
          loop,
          CReturnStmt(Some(setDecl.ref))
        ))

        CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }
}
