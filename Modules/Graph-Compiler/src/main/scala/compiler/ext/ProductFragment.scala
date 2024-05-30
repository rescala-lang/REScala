package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.*
import clangast.expr.*
import clangast.stmt.*
import clangast.stubs.{CJSONH, StdBoolH, StdIOH, StdLibH, StringH}
import clangast.types.*
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.DataStructureFragment.{release, retain}
import compiler.context.{RecordDeclTC, TranslationContext}
import compiler.ext.SerializationFragment.{deserialize, serialize}

import scala.quoted.*

object ProductFragment extends SelectIFFragment with ApplyIFFragment with MatchIFFragment with TypeIFFragment
    with DataStructureIFFragment with StringIFFragment with SerializationIFFragment {
  override def compileSelect(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case Select(qualifier, name) if isProductFieldAccess(qualifier, name) =>
        CMemberExpr(dispatch[TermIFFragment](_.compileTermToCExpr)(qualifier), name)
    }
  }

  override def compileApply(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case apply @ Apply(Select(_, "apply"), l) if isProductApply(apply) =>
        CCallExpr(getProductCreator(apply.tpe).ref, l.map(dispatch[TermIFFragment](_.compileTermToCExpr)))
      case apply @ Apply(TypeApply(Select(_, "apply"), _), l) if isProductApply(apply) =>
        CCallExpr(getProductCreator(apply.tpe).ref, l.map(dispatch[TermIFFragment](_.compileTermToCExpr)))
      case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(prod)), List()) if prod.tpe <:< TypeRepr.of[Product] =>
        CCallExpr(
          getProductDeepCopy(prod.tpe).ref,
          List(dispatch[TermIFFragment](_.compileTermToCExpr)(prod))
        )
    }
  }

  override def compileEquals(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Product] =>
          CCallExpr(
            getProductEquals(leftType).ref,
            List(leftExpr, rightExpr)
          )
      }
    }

  override def compilePattern(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (Unapply(_, _, subPatterns), prefix, prefixType) if prefixType <:< TypeRepr.of[Product] =>
          val fieldSym       = fieldSymbols(prefixType)
          val subPrefixes    = fieldSym.map(fs => CMemberExpr(prefix, fs.name.strip()))
          val subPrefixTypes = fieldSym.map(prefixType.memberType)

          (subPatterns zip (subPrefixes zip subPrefixTypes)).foldLeft((Option.empty[CExpr], List.empty[CVarDecl])) {
            case ((cond, decls), (subPattern, (subPrefix, subPrefixType))) =>
              val (subCond, subDecls) =
                dispatch[MatchIFFragment](_.compilePattern)(subPattern, subPrefix, subPrefixType)

              val combinedCond = MatchFragment.combineCond(cond, subCond)

              (combinedCond, subDecls ++ decls)
          }
      }
    }

  override def compileTypeRepr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] =>
        getRecordDecl(tpe).getTypeForDecl
    }
  }

  override def typeName(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] => dispatch[TypeIFFragment](_.classTypeName)(tpe)
    }
  }

  override def defaultValue(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] && hasDefaultValue(tpe) =>
        CCallExpr(
          getProductCreator(tpe).ref,
          fieldSymbols(tpe).map(tpe.memberType).map(dispatch[TypeIFFragment](_.defaultValue))
        )
    }
  }

  override def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] =>
        val fields = fieldSymbols(tpe).map { symbol =>
          CFieldDecl(symbol.name.strip(), dispatch[TypeIFFragment](_.compileTypeRepr)(tpe.memberType(symbol)))
        }

        val refCountFieldDecl =
          if dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) then
            List(CFieldDecl(refCountFieldName, CPointerType(CIntegerType)))
          else Nil

        CRecordDecl(dispatch[TypeIFFragment](_.typeName)(tpe), fields ++ refCountFieldDecl)
    }
  }

  override def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] =>
        fieldTypes(tpe).exists(dispatch[DataStructureIFFragment](_.usesRefCount))
    }
  }

  override def compileFree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Product] =>
        val recordDecl = getRecordDecl(tpe)

        val releaseElems: List[CStmt] = recordDecl.fields.zip(fieldTypes(tpe)).collect {
          case (f, t) if dispatch[DataStructureIFFragment](_.usesRefCount)(t) =>
            release(CMemberExpr(expr, f.name), t, CFalseLiteral).get
        }

        val freeRefCount = CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountFieldName)))

        CCompoundStmt(releaseElems :+ freeRefCount)
    }
  }

  override def compileDeepCopy(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] && dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) =>
        getProductDeepCopy(tpe)
    }
  }

  override def compilePrint(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Product] =>
        CCallExpr(getProductPrinter(tpe).ref, List(expr))
    }
  }

  override def compileToString(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Product] =>
        CCallExpr(getProductToString(tpe).ref, List(expr))
    }
  }

  override def serializationRetainsEquality(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] =>
        fieldTypes(tpe).forall(dispatch[SerializationIFFragment](_.serializationRetainsEquality))
    }
  }

  override def compileSerialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] =>
        getProductSerialize(tpe)
    }
  }

  override def compileDeserialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Product] =>
        getProductDeserialize(tpe)
    }
  }

  private val CREATE    = "CREATE"
  private val EQUALS    = "EQUALS"
  private val PRINT     = "PRINT"
  private val TO_STRING = "TO_STRING"

  private def isProductApply(using Quotes)(apply: quotes.reflect.Apply): Boolean = {
    import quotes.reflect.*

    apply match {
      case Apply(Select(i, "apply"), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case Apply(TypeApply(Select(i, "apply"), _), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case _ => false
    }
  }

  private def fieldSymbols(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.Symbol] =
    tpe.classSymbol.get.caseFields.filter(_.isValDef)

  private def fieldTypes(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    fieldSymbols(tpe).map(tpe.memberType)

  private def hasDefaultValue(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      TranslationContext
  ): Boolean =
    !dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) &&
    fieldSymbols(tpe).map(tpe.memberType).forall(TypeFragment.hasDefaultValue)

  private def isProductFieldAccess(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    (term.tpe <:< TypeRepr.of[Product]) && fieldSymbols(term.tpe).exists(_.name.strip().equals(name))
  }

  def makeProductCreator(recordDecl: CRecordDecl) = {
    val name = "create_" + recordDecl.name

    val parameters = recordDecl.fields.map { field =>
      CParmVarDecl(field.name, field.declaredType)
    }

    // TODO: this ignores reference counting
    val create = CDesignatedInitExpr(recordDecl.fields.map { field => field.name } zip parameters.map(p => p.ref))

    val prodDecl = CVarDecl(
      "prod",
      recordDecl.getTypeForDecl,
      Some(create)
    )
    val body = CCompoundStmt(List(
      prodDecl,
      CReturnStmt(Some(prodDecl.ref))
    ))

    CFunctionDecl(name, parameters, recordDecl.getTypeForDecl, Some(body))
  }

  private def getProductCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> CREATE, {
        val recordDecl = getRecordDecl(tpe)
        val name       = "create_" + recordDecl.name

        val parameters = fieldSymbols(tpe).map { fs =>
          val fieldName = fs.name.strip()
          CParmVarDecl(fieldName, recordDecl.getField(fieldName).declaredType)
        }

        val prodDecl = CVarDecl(
          "prod",
          recordDecl.getTypeForDecl,
          Some(CDesignatedInitExpr(
            (fieldSymbols(tpe) zip parameters).map {
              (fs: quotes.reflect.Symbol, p: CParmVarDecl) => (p.name, retain(p.ref, tpe.memberType(fs)))
            } ++ allocRefCount(tpe)
          ))
        )
        val body = CCompoundStmt(List(
          prodDecl,
          CReturnStmt(Some(prodDecl.ref))
        ))

        CFunctionDecl(name, parameters, recordDecl.getTypeForDecl, Some(body))
      }
    )
  }

  private def getProductEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      getRecordDecl(tpe).name -> EQUALS, {
        val recordDecl = getRecordDecl(tpe)

        val name = "equals_" + recordDecl.name

        val paramLeft  = CParmVarDecl("left", recordDecl.getTypeForDecl)
        val paramRight = CParmVarDecl("right", recordDecl.getTypeForDecl)
        val parameters = List(paramLeft, paramRight)

        val memberEquals: List[CExpr] = fieldSymbols(tpe).map { symbol =>
          val memberType = tpe.memberType(symbol)
          dispatch[ApplyIFFragment](_.compileEquals)(
            CMemberExpr(paramLeft.ref, symbol.name.strip()),
            memberType,
            CMemberExpr(paramRight.ref, symbol.name.strip()),
            memberType
          )
        }

        val equalsExpr = memberEquals.reduceOption(CAndExpr.apply).getOrElse(CTrueLiteral)

        val body = CCompoundStmt(List(CReturnStmt(Some(equalsExpr))))

        CFunctionDecl(name, parameters, StdBoolH.bool, Some(body))
      }
    )
  }

  private def getProductDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DEEP_COPY, {
        val recordDecl = getRecordDecl(tpe)

        val name         = "deepCopy_" + recordDecl.name
        val productParam = CParmVarDecl("rec", recordDecl.getTypeForDecl)

        val copy = CCallExpr(
          getProductCreator(tpe).ref,
          fieldSymbols(tpe) map { fs =>
            val fieldType = tpe.memberType(fs)
            DataStructureFragment.deepCopy(
              CMemberExpr(productParam.ref, fs.name.strip()),
              fieldType
            )
          }
        )

        val body = CCompoundStmt(List(
          CReturnStmt(Some(copy))
        ))

        CFunctionDecl(
          name,
          List(productParam),
          recordDecl.getTypeForDecl,
          Some(body)
        )
      }
    )
  }

  private def getProductPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT, {
        val recordDecl = getRecordDecl(tpe)

        val name         = "print_" + recordDecl.name
        val productParam = CParmVarDecl("rec", recordDecl.getTypeForDecl)

        val printFields = recordDecl.fields.zip(fieldTypes(tpe)).flatMap { (f, t) =>
          val printField = dispatch[StringIFFragment](_.compilePrint)(
            CMemberExpr(productParam.ref, f.name),
            t
          )

          List[CStmt](
            StringFragment.printf(", "),
            printField
          )
        }.tail

        val body = CCompoundStmt(List(
          StringFragment.printf(if recordDecl.name.startsWith("Tuple") then "(" else tpe.classSymbol.get.name + "("),
          CCompoundStmt(printFields),
          StringFragment.printf(")")
        ))

        CFunctionDecl(
          name,
          List(productParam),
          CVoidType,
          Some(body)
        )
      }
    )
  }

  private def getProductToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> TO_STRING, {
        import quotes.reflect.*

        val recordDecl = getRecordDecl(tpe)
        val numFields  = recordDecl.fields.length

        val isTuple = recordDecl.name.startsWith("Tuple")

        val name         = "toString_" + recordDecl.name
        val productParam = CParmVarDecl("rec", recordDecl.getTypeForDecl)

        val fieldStringsDecl = CVarDecl(
          "fieldStrings",
          CArrayType(CPointerType(CCharType), Some(CIntegerLiteral(numFields)))
        )

        val baseLength = if isTuple then 2 else 2 + tpe.classSymbol.get.name.length

        val stringLengthDecl = CVarDecl(
          "strLength",
          CIntegerType,
          Some(CIntegerLiteral(baseLength + 2 * (numFields - 1)))
        )

        val fieldsToString: List[CStmt] = recordDecl.fields.zip(fieldTypes(tpe)).zipWithIndex.flatMap {
          case ((f, t), i) =>
            val fieldToString = CAssignmentExpr(
              CArraySubscriptExpr(fieldStringsDecl.ref, i.lit),
              dispatch[StringIFFragment](_.compileToString)(
                CMemberExpr(productParam.ref, f.name.strip()),
                t
              )
            )

            val addFieldStringLength = CPlusAssignmentExpr(
              stringLengthDecl.ref,
              CCallExpr(StringH.strlen.ref, List(CArraySubscriptExpr(fieldStringsDecl.ref, i.lit)))
            )

            List(fieldToString, addFieldStringLength)
        }

        val strDecl = StringFragment.stringDecl("str", stringLengthDecl.ref)

        val formatString = (if isTuple then "(" else tpe.classSymbol.get.name + "(") +
          fieldTypes(tpe).map(_ => "%s").mkString(", ") + ")"

        val sprintf = CCallExpr(
          StdIOH.sprintf.ref,
          strDecl.ref ::
          CStringLiteral(formatString) ::
          (0 until numFields).indices.map(i => CArraySubscriptExpr(fieldStringsDecl.ref, i.lit)).toList
        )

        val freeFieldStrings = (0 until numFields).map[CStmt](i =>
          CCallExpr(StdLibH.free.ref, List(CArraySubscriptExpr(fieldStringsDecl.ref, i.lit)))
        ).toList

        val body = CCompoundStmt(List(
          fieldStringsDecl,
          stringLengthDecl,
          CCompoundStmt(fieldsToString),
          strDecl,
          sprintf,
          CCompoundStmt(freeFieldStrings),
          CReturnStmt(Some(strDecl.ref))
        ))

        CFunctionDecl(
          name,
          List(productParam),
          CPointerType(CCharType),
          Some(body)
        )
      }
    )
  }

  private def getProductSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE, {
        val recordDecl = getRecordDecl(tpe)
        val isTuple    = recordDecl.name.startsWith("Tuple")

        val name = "serialize_" + recordDecl.name

        val prodParam = CParmVarDecl("prod", recordDecl.getTypeForDecl)

        val body = if isTuple then {
          val jsonDecl =
            CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))
          val addFields = recordDecl.fields.zip(fieldTypes(tpe)).map[CStmt] { (field, tpe) =>
            CCallExpr(
              CJSONH.cJSON_AddItemToArray.ref,
              List(jsonDecl.ref, serialize(CMemberExpr(prodParam.ref, field.name), tpe))
            )
          }
          CCompoundStmt((CDeclStmt(jsonDecl) :: addFields) :+ CReturnStmt(Some(jsonDecl.ref)))
        } else {
          val jsonDecl =
            CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateObject.ref, List())))
          val addFields = recordDecl.fields.zip(fieldTypes(tpe)).map[CStmt] { (field, tpe) =>
            CCallExpr(
              CJSONH.cJSON_AddItemToObject.ref,
              List(jsonDecl.ref, CStringLiteral(field.name), serialize(CMemberExpr(prodParam.ref, field.name), tpe))
            )
          }
          CCompoundStmt((CDeclStmt(jsonDecl) :: addFields) :+ CReturnStmt(Some(jsonDecl.ref)))
        }

        CFunctionDecl(name, List(prodParam), CPointerType(CJSONH.cJSON), Some(body))
      }
    )
  }

  private def getProductDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DESERIALIZE, {
        val recordDecl = getRecordDecl(tpe)
        val isTuple    = recordDecl.name.startsWith("Tuple")

        val name = "deserialize_" + recordDecl.name

        val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

        val params = if isTuple then {
          fieldTypes(tpe).zipWithIndex map { (tpe, i) =>
            deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonParam.ref, CIntegerLiteral(i))), tpe)
          }
        } else {
          recordDecl.fields.zip(fieldTypes(tpe)) map { (field, tpe) =>
            deserialize(CCallExpr(CJSONH.cJSON_GetObjectItem.ref, List(jsonParam.ref, CStringLiteral(field.name))), tpe)
          }
        }

        val body = CCompoundStmt(List(CReturnStmt(Some(CCallExpr(getProductCreator(tpe).ref, params)))))

        CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }
}
