package loci
package utility

import reflectionExtensions.*

import java.util
import scala.collection.mutable
import scala.quoted.*
import scala.util.control.NonFatal

object implicitHints:
  private given Ordering[String] with
    def compare(x: String, y: String): Int =
      val length = math.min(x.length, y.length)
      var i = 0

      while i < length do
        val xi = x(i)
        val yi = y(i)

        if Character.isLowerCase(xi) && Character.isUpperCase(yi) then
          return -1
        if Character.isUpperCase(xi) && Character.isLowerCase(yi) then
          return 1
        if xi != yi then
          return xi - yi

        i += 1

      x.length - y.length
  end given

  private val fileUtils =
    try
      val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
      val symbolClass = Class.forName("dotty.tools.dotc.core.Symbols$Symbol")
      val abstractFileClass = Class.forName("dotty.tools.io.AbstractFile")

      Some(
        (symbolClass.getMethod("associatedFile", contextClass),
         abstractFileClass.getMethod("path"),
         abstractFileClass.getMethod("underlyingSource")))
    catch
      case _: ClassNotFoundException | _: NoSuchMethodException =>
        None

  private def mightBeScalaSource(using Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    import quotes.reflect.*

    try
      fileUtils forall { (associatedFileMethod, pathMethod, underlyingSourceMethod) =>
        def mightBeScalaSource(file: Any): Boolean =
          if file == null then
            return true

          val path = pathMethod.invoke(file) match
            case path: String => path
            case _ => return true

          if path.isEmpty || (path contains "scala-") then
            return true

          underlyingSourceMethod.invoke(file) match
            case Some(underlyingSource)
                if underlyingSource != file &&
                   mightBeScalaSource(underlyingSource) =>
              return true
            case _ =>

          var i = path.length - 5

          if i <= 1 || path(i + 1) != '.' || path(i + 2) != 'j' || path(i + 3) != 'a' || path(i + 4) != 'r' then
            return false

          while
            val c = path(i)
            if c != '.' && (c < '0' || c > '9') then
              return false
            i -= 1
            i > 1 && path(i) != '-'
          do ()

          i -= 1

          while
            val c = path(i)
            if c != '.' && (c < '0' || c > '9') then
              return false
            i -= 1
            i > 1 && path(i) != '_'
          do ()

          i > 1 && (path(i + 1) == '2' || path(i + 1) == '3') && (path(i + 2) == '.' || path(i + 2) == '-')
        end mightBeScalaSource

        mightBeScalaSource(associatedFileMethod.invoke(symbol, quotes.getClass.getMethod("ctx").invoke(quotes)))
      }
    catch
      case _: NoSuchMethodException | _: IllegalArgumentException =>
        true
  end mightBeScalaSource

  private val importNamesCache = new util.WeakHashMap[Any, Set[String]]

  private val parserUtils =
    try
      val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
      val sourceFileClass = Class.forName("dotty.tools.dotc.util.SourceFile")
      val parsersClass = Class.forName("dotty.tools.dotc.parsing.Parsers")
      val parserClass = Class.forName("dotty.tools.dotc.parsing.Parsers$Parser")
      val importClass = Class.forName("dotty.tools.dotc.ast.Trees$Import")
      val nameClass = Class.forName("dotty.tools.dotc.core.Names$Name")

      Some(
        (parsersClass.getMethod("parser", sourceFileClass, contextClass),
         parserClass.getMethod("compilationUnit"),
         importClass,
         nameClass))
    catch
      case _: ClassNotFoundException | _: NoSuchMethodException =>
        None

  private def importNames(using Quotes)(file: quotes.reflect.SourceFile) =
    import quotes.reflect.*

    importNamesCache.get(file) match
      case names if names != null =>
        names

      case _ =>
        val names =
          try
            parserUtils.to(Set) flatMap { (parserMethod, compilationUnitMethod, importClass, nameClass) =>
              val parser = parserMethod.invoke(null, file, quotes.getClass.getMethod("ctx").invoke(quotes))
              val tree = compilationUnitMethod.invoke(parser)

              def importNames(insideImportScope: Boolean)(tree: Any): Set[String] = tree match
                case name if insideImportScope && nameClass.isInstance(name) =>
                  Set(name.toString)
                case trees: List[?] =>
                  trees.toSet flatMap importNames(insideImportScope)
                case tree: Product =>
                  tree.productIterator.toSet flatMap importNames(insideImportScope || importClass.isInstance(tree))
                case _ =>
                  Set.empty

              importNames(insideImportScope = false)(tree) - "_" - ""
            }
          catch
            case _: NoSuchMethodException | _: IllegalArgumentException =>
              Set.empty

        importNamesCache.put(file, names)
        names
  end importNames

  private def typeSymbolNames(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect.*

    val symbols =
      try
        val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")

        def typeSymbols(tpe: Any): Set[Symbol] =
          val symbols =
            tpe match
              case tpes: List[?] => tpes.toSet flatMap typeSymbols
              case tpe: Product => tpe.productIterator.toSet flatMap typeSymbols
              case _ => Set.empty

          tpe match
            case tpe: TypeRepr @unchecked if typeClass.isInstance(tpe) =>
              symbols + tpe.typeSymbol
            case _ =>
              symbols

        typeSymbols(tpe)

      catch
        case _: ClassNotFoundException | _: ClassCastException =>
          Set(tpe.typeSymbol)

    symbols flatMap { symbol =>
      (ancestors(symbol)
        filter { !_.isTypeParam }
        map { _.name }
        filter { name =>
          name.nonEmpty &&
          (name.head != '<' || name.last != '>') &&
          !(name contains '$')
        })
    }
  end typeSymbolNames

  private def fieldMembers(using Quotes)(symbol: quotes.reflect.Symbol) =
    try symbol.fieldMembers catch { case NonFatal(_) => List.empty }

  private def methodMembers(using Quotes)(symbol: quotes.reflect.Symbol) =
    try symbol.methodMembers catch { case NonFatal(_) => List.empty }

  private def findImplicits(using Quotes)(
      tpe: Option[quotes.reflect.TypeRepr],
      findConversions: Boolean) =
    import quotes.reflect.*

    val (maxDepth, maxVisited, resetDepth) =
      (tpe
        map { tpe =>
          val related = typeSymbolNames(tpe) ++ importNames(Position.ofMacroExpansion.sourceFile)
          val normalRelated = related map { _.toLowerCase }
          val resetDepth = { (name: String) =>
            val normalName = name.toLowerCase
            normalRelated exists { normalRelated =>
              if normalRelated.length < 4 || normalName.length < 4 then
                normalRelated == normalName
              else
                (normalRelated contains normalName) || (normalName contains normalRelated)
            }
          }

          (2, 5000, resetDepth)
        }
        getOrElse
          (5, 5000, (_: String) => false))

    val implicitConversions = mutable.ListBuffer.empty[Select]
    val implicitValues = mutable.ListBuffer.empty[Select]
    val queue = mutable.Queue.empty[(Ref, Int)]
    val visited = new util.HashSet[Symbol]

    queue.enqueue(Ref(defn.RootPackage) -> 0)

    while queue.nonEmpty do
      val (ref, depth) = queue.dequeue()
      val symbol = ref.symbol

      if visited.add(symbol) && mightBeScalaSource(symbol) then
        fieldMembers(symbol) ++ methodMembers(symbol) foreach { member =>
          try
            val root = symbol == defn.RootPackage
            val name = member.name
            val knownPackage =
              root && (name match {
                case "java" | "javax" | "jdk" | "netscape" | "sun" => true
                case _ => false
              }) ||
              (name == "sun" &&
               member.owner.name == "com" &&
               member.owner.owner == defn.RootClass)

            if !knownPackage &&
               member.isTerm &&
               member.isPublic &&
               !(member.name contains '$') then

              val selection = ref.select(member)

              if member.isModuleDef &&
                 depth < maxDepth &&
                 visited.size < maxVisited then
                queue.enqueue(selection -> (if resetDepth(member.name) then 0 else depth + 1))

              if symbol != defn.ScalaPackage &&
                 symbol != defn.PredefModule then

                if member.isExtensionMethod && findConversions then
                  implicitConversions += selection

                if member.isImplicit && isMeaningfulType(selection.tpe.widenTermRefByName.finalResultType) then
                  val nonImplicitValues = member.paramSymss.flatten count { arg =>
                    !arg.isTypeParam && !arg.isImplicit
                  }

                  if nonImplicitValues == 0 then
                    implicitValues += selection
                  else if nonImplicitValues == 1 && findConversions then
                    implicitConversions += selection
          catch
            case NonFatal(_) =>
        }
    end while

    implicitValues.toList -> implicitConversions.toList
  end findImplicits

  private def isMeaningfulType(using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect.*

    tpe match
      case _: ParamRef => false
      case _ =>
        val symbol = tpe.typeSymbol
        symbol != defn.AnyClass &&
        symbol != defn.AnyValClass &&
        symbol != defn.AnyRefClass &&
        symbol != defn.ObjectClass &&
        symbol != defn.NothingClass &&
        symbol != defn.NullClass

  private def isMeaningfulMethod(using Quotes)(symbol: quotes.reflect.Symbol) =
    import quotes.reflect.*

    symbol.isTerm &&
    symbol.isPublic &&
    !symbol.isClassConstructor &&
    !symbol.isExtensionMethod &&
    !symbol.isImplicit &&
    !(symbol.name contains '$') &&
    !(symbol.name endsWith "_=") &&
    (symbol :: symbol.allOverriddenSymbols.toList forall { symbol =>
      !(Seq(
        defn.AnyClass,
        defn.AnyValClass,
        defn.AnyRefClass,
        defn.ProductClass,
        defn.ObjectClass) contains symbol.owner)
    })

  private def ancestors(using Quotes)(symbol: quotes.reflect.Symbol): List[quotes.reflect.Symbol] =
    if symbol.exists then
      symbol :: ancestors(symbol.owner)
    else
      List.empty

  private def associatedImplicitScope(using Quotes)(
      tpe: quotes.reflect.TypeRepr): Set[quotes.reflect.Symbol] =
    import quotes.reflect.*

    def owner(tpe: TypeRepr): Symbol = tpe match {
      case TypeRef(qualifier, _) => qualifier.typeSymbol
      case tpe => tpe.typeSymbol.owner
    }

    val prefixes =
      ancestors(owner(tpe)) ++ ancestors(owner(tpe.dealias)) collect {
        case symbol if !symbol.isPackageDef => symbol.companionModule
      }

    val baseCompanions = tpe.dealias.baseClasses.distinct map { _.companionModule }

    val implicitScope = (baseCompanions ++ prefixes).toSet - Symbol.noSymbol

    implicitScope ++ (tpe.typeArgs flatMap associatedImplicitScope)
  end associatedImplicitScope

  private def associatedImplicitScopeExtension(using Quotes)(
      scope: Set[quotes.reflect.Symbol]) =
    scope flatMap { symbol => fieldMembers(symbol) filter { _.isImplicit } }

  private enum Variance:
    case Covariant, Contravariant, Invariant

  private object Variance:
    def apply(using Quotes)(flags: quotes.reflect.Flags) =
      import quotes.reflect.*
      if flags is Flags.Covariant then Covariant
      else if flags is Flags.Contravariant then Contravariant
      else Invariant

  private def typeInstantiations(using Quotes)(
      pattern: quotes.reflect.TypeRepr,
      scrutinee: quotes.reflect.TypeRepr,
      variance: Variance): Option[Map[quotes.reflect.ParamRef, quotes.reflect.TypeRepr]] =
    import quotes.reflect.*

    val patternResult = pattern.finalResultType
    val scrutineeResult = scrutinee.finalResultType

    val parameter = patternResult.typeConstructor match {
      case param: ParamRef => Some(param)
      case _ => None
    }

    val covariant = variance == Variance.Covariant &&
      (patternResult.baseClasses contains scrutineeResult.typeSymbol)

    val contravariant = variance == Variance.Contravariant &&
      (scrutineeResult.baseClasses contains patternResult.typeSymbol)

    val invariant = variance == Variance.Invariant &&
      (patternResult.baseClasses contains scrutineeResult.typeSymbol) &&
      (scrutineeResult.baseClasses contains patternResult.typeSymbol)

    if parameter.nonEmpty || covariant || contravariant || invariant then
      val patternBase = if covariant then patternResult.baseType(scrutineeResult.typeSymbol) else patternResult
      val scrutineeBase = if contravariant then scrutineeResult.baseType(patternResult.typeSymbol) else scrutineeResult

      val patternArgs = patternBase.typeArgs
      val scrutineeArgs = scrutineeBase.typeArgs

      if parameter.nonEmpty && patternArgs.isEmpty then
        Some(Map(parameter.get -> scrutineeResult))
      else if patternArgs.size == scrutineeArgs.size then
        ((patternArgs zip scrutineeArgs zip patternBase.typeConstructor.typeArgVariances)
          .foldLeft[Option[Map[ParamRef, TypeRepr]]](Some(Map.empty)) {
            case (instantiations, ((pattern, scrutinee), variance)) =>
              if !(scrutinee =:= TypeBounds.empty) then
                instantiations flatMap { instantiations =>
                  typeInstantiations(pattern, scrutinee, Variance(variance)) map {
                    _ ++ instantiations
                  }
                }
              else
                instantiations
          }
          map { instantiations =>
            if parameter.nonEmpty then
              instantiations + (parameter.get -> scrutineeResult.typeConstructor)
            else
              instantiations
          })
      else
        None
    else
      None
  end typeInstantiations

  private def instantiateType(using Quotes)(
      tpe: quotes.reflect.TypeRepr,
      instantiations: Map[quotes.reflect.ParamRef, quotes.reflect.TypeRepr]) =
    import quotes.reflect.*

    tpe match
      case tpe: PolyType =>
        val params = tpe.paramTypes.indices.toList map { index =>
          val param @ ParamRef(_, _) = tpe.param(index): @unchecked
          instantiations get param toRight (tpe.paramNames(index) -> tpe.paramBounds(index) -> param)
        }

        val (names, bounds) = (params collect { case Left((param, _)) => param }).unzip

        if names.nonEmpty then
          val polyType =
            PolyType(names)(_ => bounds, { self =>
              val (args, _) = params.foldLeft(List.empty[TypeRepr] -> 0) {
                case ((args, index), Right(tpe)) => (args :+ tpe) -> index
                case ((args, index), Left(_)) => (args :+ self.param(index)) -> (index + 1)
              }
              tpe.appliedTo(args)
            })

          val (args, _) = params.foldLeft(List.empty[(ParamRef, TypeRepr)] -> 0) {
            case ((args, index), Right(_)) => args -> index
            case ((args, index), Left((_, param))) => (param -> polyType.param(index) :: args) -> (index + 1)
          }

          (args ++ instantiations.toList).foldLeft[TypeRepr](polyType) { case (tpe, (from, to)) =>
            tpe.substitute(from, to)
          }

        else
          tpe.appliedTo(params collect { case Right(tpe) => tpe })

      case _ =>
        tpe
  end instantiateType

  private def substituteTypeParams(using Quotes)(
      tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect.*
    tpe match
      case tpe: PolyType => tpe.appliedTo(tpe.paramBounds)
      case _ => tpe

  private def mightBeResolvable(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    var inst = tpe
    var args = implicitArgumentTypes(inst)

    val size = args.size
    var index = 0

    while index < size do
      Implicits.search(implicitArgumentTypes(substituteTypeParams(inst))(index)) match
        case result: ImplicitSearchSuccess =>
          val instantiations = typeInstantiations(args(index), result.tree.tpe, Variance.Contravariant)
          if instantiations.isEmpty then
            return false

          inst = instantiateType(inst, instantiations.get)
          args = implicitArgumentTypes(inst)

        case _ =>
          return false

      index += 1
    end while

    return true
  end mightBeResolvable

  private def implicitArgumentTypes(using Quotes)(
      tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tpe match
      case tpe: PolyType =>
        implicitArgumentTypes(tpe.resType)
      case tpe: MethodType =>
        val args = if tpe.isImplicit then tpe.paramTypes else List.empty
        args ++ implicitArgumentTypes(tpe.resType)
      case _ if tpe.isContextFunctionType =>
        val args = tpe.typeArgs
        args.dropRight(1) ++ (args.lastOption.toList flatMap implicitArgumentTypes)
      case _ =>
        List.empty

  private def firstNonImplicitArgumentType(using Quotes)(
      tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tpe match
      case tpe: PolyType =>
        firstNonImplicitArgumentType(tpe.resType)
      case tpe: MethodType =>
        if tpe.isImplicit then
          firstNonImplicitArgumentType(tpe.resType)
        else if tpe.paramTypes.size == 1 then
          Some(tpe.paramTypes.head)
        else
          None
      case _ if tpe.isContextFunctionType =>
        tpe.typeArgs.lastOption flatMap firstNonImplicitArgumentType
      case _ =>
        None

  private def importStatement(using Quotes)(ref: quotes.reflect.Select) =
    import quotes.reflect.*

    val base =
      if ref.symbol.isExtensionMethod && ref.qualifier.symbol.isImplicit then
        ref.qualifier
      else
        ref

    def makePath(term: Term): List[String] = term match
      case Select(qualifier, name) =>
        if term.symbol == base.symbol ||
           qualifier.symbol.isPackageDef && name == "package" then
          makePath(qualifier)
        else
          makePath(qualifier) :+ name
      case _ if term.symbol == defn.RootPackage =>
        List.empty
      case _ =>
        val path = term.safeShow("")
        if path.nonEmpty then List(path) else List.empty

    val path = makePath(base)
    path.size -> path.mkString("import ", ".", if base.symbol.isExtensionMethod then ".*" else ".given")
  end importStatement

  private def relatedImportStatements(using Quotes)(
      implicitValues: List[quotes.reflect.Select],
      ref: quotes.reflect.Ref,
      tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect.*

    val result = mutable.ListBuffer.empty[(Int, (Int, String))]
    val queue = mutable.Queue.empty[(Ref, TypeRepr, Int)]
    val visited = new util.HashSet[Ref]

    queue.enqueue((ref, tpe, 0))

    while queue.nonEmpty do
      val (ref, tpe, level) = queue.dequeue()

      if visited.add(ref) then
        val refType = ref.tpe.widenTermRefByName

        typeInstantiations(refType, tpe, Variance.Covariant) foreach { instantiations =>
          implicitArgumentTypes(instantiateType(refType, instantiations)) foreach { tpe =>
            if isMeaningfulType(tpe) then
              implicitValues foreach { ref =>
                val refType = ref.tpe.widenTermRefByName

                if isMeaningfulType(refType.finalResultType) then
                  typeInstantiations(refType, tpe, Variance.Covariant) foreach { _ =>
                    queue.enqueue((ref, tpe, level + 1))

                    if !(associatedImplicitScope(tpe) contains ref.qualifier.symbol) then
                      result += level -> importStatement(ref)
                  }
              }
          }
        }
    end while

    result.result()
  end relatedImportStatements

  private def hints(using Quotes)(tpe: quotes.reflect.TypeRepr, tpeIsBaseOfConversion: Boolean) =
    import quotes.reflect.*

    val tpeOuter = tpe

    val result = noReporting(List.empty) {
      import quotes.reflect.*

      val tpe = tpeOuter match
        case tpe: TypeRepr @unchecked => tpe

      val (implicitValues, implicitConversions) = findImplicits(Some(tpe), tpeIsBaseOfConversion)
      val scope = associatedImplicitScope(tpe)
      val scopeExtension = associatedImplicitScopeExtension(scope)

      val implicits =
        if tpeIsBaseOfConversion then
          implicitConversions flatMap { ref =>
            val refType = ref.tpe.widenTermRefByName
            val baseType = firstNonImplicitArgumentType(refType)

            baseType flatMap {
              case baseType if isMeaningfulType(baseType) && isMeaningfulType(refType.finalResultType) =>
                typeInstantiations(baseType, tpe, Variance.Contravariant) map { instantiations =>
                  (ref, instantiateType(refType, instantiations))
                }
              case _ =>
                None
            }
          }
        else
          implicitValues flatMap { ref =>
            val refType = ref.tpe.widenTermRefByName

            if isMeaningfulType(refType.finalResultType) then
              typeInstantiations(refType, tpe, Variance.Covariant) map { instantiations =>
                (ref, instantiateType(refType, instantiations))
              }
            else
              None
          }

      val sortedEntries = ((implicits
        map { (ref, tpe) =>
          val inImplicitScope =
            (scope contains ref.qualifier.symbol) ||
            ref.symbol.isExtensionMethod && (scopeExtension contains ref.qualifier.symbol)

          (ref, tpe, if !inImplicitScope then importStatement(ref) else 0 -> "")
        }
        groupBy { case (_, _, importStatement) => importStatement }).toList
        sortBy { case (importStatement, _) => importStatement })

      val selectedEntries =
        if tpeIsBaseOfConversion then
          sortedEntries map { case entry @ (importStatement, entries) =>
            val typeCheckingEntries = entries filter { (_, tpe, _) => mightBeResolvable(tpe) }

            if typeCheckingEntries.nonEmpty then
              (importStatement, typeCheckingEntries)
            else
              entry
          }
        else
          sortedEntries

      selectedEntries map { case ((_, importStatement), entries) =>
        val importStatements =
          ((entries flatMap { (ref, tpe, _) =>
            relatedImportStatements(implicitValues, ref, tpe)
          }).sorted map { case (_, (_, importStatement)) =>
            importStatement
          }).distinct

        val related = importStatements filterNot { _ == importStatement }

        val relatedHint =
          if related.nonEmpty then s"\n${related map { related => s"   (related `$related`)" } mkString "\n"}" else ""

        (entries, relatedHint, importStatement)
      }
    }

    result match
      case result: List[(List[(Select, TypeRepr, (Int, String))], String, String)] @unchecked => result
      case _ => List.empty
  end hints

  private def hasPrintSetting(using Quotes)(printSetting: String, defaultOnError: Boolean) =
    try
      val ctx = quotes.getClass.getMethod("ctx").invoke(quotes)
      val settings = ctx.getClass.getMethod("settings").invoke(ctx)
      val settingsState = ctx.getClass.getMethod("settingsState").invoke(ctx)
      val xprint = settings.getClass.getMethod("Xprint").invoke(settings)
      val value = xprint.getClass.getMethod("valueIn", settingsState.getClass).invoke(xprint, settingsState)

      value match
        case value: List[?] => value contains printSetting
        case _ => defaultOnError

    catch
      case _: ClassNotFoundException | _: NoSuchMethodException | _: IllegalArgumentException =>
        defaultOnError

  def extensions(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*

    if !hasPrintSetting("loci.macro.no-implicit-hints", true) then
      val hint =
        (hints(tpe, tpeIsBaseOfConversion = true)
          map { (entries, relatedHint, importStatement) =>
            val identifiers = (entries flatMap { (ref, _, _) =>
              if ref.symbol.isExtensionMethod then
                List(ref.name)
              else
                val symbol = ref.tpe.widenTermRefByName.finalResultType.typeSymbol
                fieldMembers(symbol) ++ methodMembers(symbol) collect {
                  case symbol if isMeaningfulMethod(symbol) =>
                    symbol.name
                }
            }).distinct.sorted

            if identifiers.nonEmpty then
              val importHint = if importStatement.nonEmpty then s"\n   (via `$importStatement`)" else ""
              s"\n\nHint: You may use one of the following:\n   ${identifiers mkString ", "}$importHint$relatedHint"
            else
              ""
          }).mkString

      if hint.nonEmpty then s"$hint\n " else hint
    else
      ""
  end extensions

  def values(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
    if !hasPrintSetting("loci.macro.no-implicit-hints", true) then
      val hint =
        (hints(tpe, tpeIsBaseOfConversion = false)
          collect { case (_, relatedHint, importStatement) if importStatement.nonEmpty =>
            s"\n- `$importStatement`$relatedHint"
          }).mkString

      if hint.nonEmpty then s"\n\nHint: You may consider the following imports:$hint\n " else hint
    else
      ""
  end values
end implicitHints
