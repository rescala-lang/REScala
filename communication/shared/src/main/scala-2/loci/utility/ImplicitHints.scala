package loci
package utility

import java.util
import scala.collection.mutable
import scala.reflect.internal.util.SourceFile
import scala.reflect.io.{AbstractFile, NoAbstractFile}
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

object implicitHints {
  private implicit object stringOrdering extends Ordering[String] {
    def compare(x: String, y: String): Int = {
      val length = math.min(x.length, y.length)
      var i = 0

      while (i < length) {
        val xi = x(i)
        val yi = y(i)

        if (Character.isLowerCase(xi) && Character.isUpperCase(yi))
          return -1
        if (Character.isUpperCase(xi) && Character.isLowerCase(yi))
          return 1
        if (xi != yi)
          return xi - yi

        i += 1
      }

      x.length - y.length
    }
  }

  private def mightBeScalaSource(file: AbstractFile): Boolean = {
    val path = file.path

    if (path.isEmpty ||
        (path contains "scala-") ||
        file.underlyingSource.nonEmpty &&
        file.underlyingSource.get != file &&
        mightBeScalaSource(file.underlyingSource.get))
      return true

    var i = path.length - 5

    if (i <= 1 || path(i + 1) != '.' || path(i + 2) != 'j' || path(i + 3) != 'a' || path(i + 4) != 'r')
      return false

    do {
      val c = path(i)
      if (c != '.' && (c < '0' || c > '9'))
        return false
      i -= 1
    }
    while (i > 1 && path(i) != '-')

    i -= 1

    do {
      val c = path(i)
      if (c != '.' && (c < '0' || c > '9'))
        return false
      i -= 1
    }
    while (i > 1 && path(i) != '_')

    i > 1 && (path(i + 1) == '2' || path(i + 1) == '3') && (path(i + 2) == '.' || path(i + 2) == '-')
  }

  private val importNamesCache = new util.WeakHashMap[SourceFile, Set[String]]

  private def importNames(c: blackbox.Context)(file: SourceFile) = {
    import c.universe._

    def erasePackageStatements(content: Array[Char]) = {
      val builder = new mutable.StringBuilder(content.length)
      val length = content.length - 7
      var i = 0

      while (i < length)
        if (content(i) == 'p' &&
            content(i + 1) == 'a' &&
            content(i + 2) == 'c' &&
            content(i + 3) == 'k' &&
            content(i + 4) == 'a' &&
            content(i + 5) == 'g' &&
            content(i + 6) == 'e' &&
            !Character.isJavaIdentifierPart(content(i + 7)) &&
            (i == 0 || !Character.isJavaIdentifierPart(content(i - 1)))) {
          builder.append("       ")
          i += 7
        }
        else {
          builder.append(content(i))
          i += 1
        }

      while (i < length + 7) {
        builder += content(i)
        i += 1
      }

      builder.toString
    }

    importNamesCache.get(file) match {
      case names if names != null =>
        names

      case _ =>
        val tree =
          try c.parse(erasePackageStatements(file.content))
          catch { case _: reflect.macros.ParseException => EmptyTree }

        val names =
          (tree collect {
            case Import(expr, selectors) =>
              (selectors map { _.name.toString }) ++
              (expr collect {
                case Select(_, name) => name.toString
                case Ident(name) => name.toString
              })
          }).flatten.toSet - termNames.WILDCARD.toString

        importNamesCache.put(file, names)

        names
    }
  }

  private def typeSymbolNames(c: blackbox.Context)(tpe: c.Type) = {
    import c.universe._

    val symbols = mutable.ListBuffer.empty[Symbol]
    tpe foreach { tpe => symbols += tpe.typeSymbol }

    (symbols flatMap { symbol =>
      (ancestors(c)(symbol)
        filter { !_.isParameter }
        map { _.name.toString }
        filter { name =>
          name.nonEmpty &&
          (name.head != '<' || name.last != '>') &&
          !(name contains '$')
        })
    }).toSet
  }

  private def findImplicits(c: blackbox.Context)(tpe: c.Type, findConversions: Boolean) = {
    import c.universe._

    val (maxDepth, maxVisited, resetDepth) =
      if (tpe != NoType) {
        val related = typeSymbolNames(c)(tpe) ++ importNames(c)(c.enclosingPosition.source)
        val normalRelated = related map { _.toLowerCase }
        val resetDepth = { name: String =>
          val normalName = name.toLowerCase
          normalRelated exists { normalRelated =>
            if (normalRelated.length < 4 || normalName.length < 4)
              normalRelated == normalName
            else
              (normalRelated contains normalName) || (normalName contains normalRelated)
          }
        }

        (2, 5000, resetDepth)
      }
      else
        (5, 5000, (_: String) => false)

    val implicitConversions = mutable.ListBuffer.empty[(MethodSymbol, List[(Symbol, Symbol)])]
    val implicitValues = mutable.ListBuffer.empty[(MethodSymbol, List[(Symbol, Symbol)])]
    val queue = mutable.Queue.empty[(Symbol, List[(Symbol, Symbol)], Int)]
    val visited = new util.HashSet[Symbol]

    val (rawInfoMethod, associatedFileMethod) =
      try {
        val symbolClass = Class.forName(s"scala.reflect.internal.Symbols$$Symbol")
        (symbolClass.getMethod("rawInfo"), symbolClass.getMethod("associatedFile"))
      }
      catch { case _: ClassNotFoundException | _: NoSuchMethodException => (null, null) }

    def rawInfo(symbol: Symbol) =
      try rawInfoMethod.invoke(symbol) match {
        case tpe: Type => tpe
        case _ => NoType
      }
      catch { case _: IllegalArgumentException => NoType }

    def associatedFile(symbol: Symbol) =
      try associatedFileMethod.invoke(symbol) match {
        case file: AbstractFile => file
        case _ => NoAbstractFile
      }
      catch { case _: IllegalArgumentException => NoAbstractFile }

    if (rawInfoMethod != null)
      queue.enqueue((c.mirror.RootClass, List.empty, 0))

    while (queue.nonEmpty) {
      val (symbol, path, depth) = queue.dequeue()

      if (visited.add(symbol) && mightBeScalaSource(associatedFile(symbol))) {
        val info =
          try if (symbol.pos == NoPosition) symbol.info else rawInfo(symbol)
          catch { case NonFatal(_) => NoType }

        info.members foreach { member =>
          val root = symbol == c.mirror.RootClass
          val name = member.name.toString
          val knownPackage =
            root && (name match {
              case "java" | "javax" | "jdk" | "netscape" | "sun" => true
              case _ => false
            }) ||
            (name == "sun" &&
             member.owner.name.toString == "com" &&
             member.owner.owner == c.mirror.RootClass)

          if (!knownPackage &&
              member.isTerm &&
              member.isPublic &&
              !member.isImplementationArtifact &&
              !(member.name.toString contains '$') &&
              info.member(member.name) != NoSymbol) {
            val currentPath = if (root) path else symbol -> member.owner :: path

            if (member.isModule && depth < maxDepth && visited.size < maxVisited)
              queue.enqueue((
                member.asModule.moduleClass,
                currentPath,
                if (resetDepth(member.name.toString)) 0 else depth + 1))

            if (member.isMethod &&
                member.isImplicit &&
                symbol != definitions.ScalaPackageClass &&
                symbol != definitions.PredefModule.moduleClass) {
              val method = member.asMethod
              val tpe = method.info.finalResultType

              if (isMeaningfulType(c)(tpe, tpe.typeSymbol))
                method.paramLists match {
                  case List() =>
                    implicitValues += method -> currentPath

                  case List(arg :: _)
                      if arg.isImplicit =>
                    implicitValues += method -> currentPath

                  case List(List(conversion))
                      if findConversions && !conversion.isImplicit =>
                    implicitConversions += method -> currentPath

                  case List(List(conversion), arg :: _)
                      if findConversions && !conversion.isImplicit && arg.isImplicit =>
                    implicitConversions += method -> currentPath

                  case _ =>
                }
            }
          }
        }
      }
    }

    implicitValues.toList -> implicitConversions.toList
  }

  private def isMeaningfulType(c: blackbox.Context)(tpe: c.Type, tpeSymbol: c.Symbol) = {
    import c.universe._

    !tpeSymbol.isParameter &&
    !(tpe =:= definitions.AnyTpe ||
      tpe =:= definitions.AnyValTpe ||
      tpe =:= definitions.AnyRefTpe ||
      tpe =:= definitions.ObjectTpe ||
      tpe =:= definitions.NothingTpe ||
      tpe =:= definitions.NullTpe)
  }

  private def isMeaningfulMethod(c: blackbox.Context)(symbol: c.Symbol) = {
    import c.universe._

    symbol.isTerm &&
    symbol.isPublic &&
    !symbol.isConstructor &&
    !symbol.isImplicit &&
    !(symbol.name.toString contains '$') &&
    !(symbol.name.decodedName.toString endsWith "_=") &&
    (symbol :: symbol.overrides forall { symbol =>
      !(Seq(
        definitions.AnyClass,
        definitions.AnyValClass,
        definitions.AnyRefClass,
        definitions.ProductClass,
        definitions.ObjectClass) contains symbol.owner)
    })
  }

  private def ancestors(c: blackbox.Context)(symbol: c.Symbol): List[c.Symbol] =
    if (symbol != c.universe.NoSymbol)
      symbol :: ancestors(c)(symbol.owner)
    else
      List.empty

  private def associatedImplicitScope(c: blackbox.Context)(tpe: c.Type): Set[c.Symbol] = {
    import c.universe._

    def owner(tpe: Type): Symbol = tpe match {
      case TypeRef(pre, _, _) => pre.typeSymbol
      case tpe => tpe.typeSymbol.owner
    }

    val prefixes = ancestors(c)(owner(tpe)) ++ ancestors(c)(owner(tpe.dealias))

    val symbol = tpe.dealias.typeSymbol

    val baseCompanions =
      if (symbol.isClass)
        symbol.asClass.baseClasses.distinct map { symbol =>
          val companion = symbol.companion
          if (companion.isModule) companion.asModule.moduleClass else companion
        }
      else
        List(symbol.companion)

    val implicitScope = (baseCompanions ++ prefixes).toSet - NoSymbol

    implicitScope ++ (tpe.typeArgs flatMap { associatedImplicitScope(c)(_) })
  }

  private object Variance extends Enumeration {
    val Covariant, Contravariant, Invariant = Value

    def apply(c: blackbox.Context)(symbol: c.universe.TypeSymbol) =
      if (symbol.isCovariant) Covariant else if (symbol.isContravariant) Contravariant else Invariant
  }

  private def typeInstantiations(c: blackbox.Context)(
      pattern: c.Type,
      scrutinee: c.Type,
      variance: Variance.Value): Option[Map[c.Symbol, c.Type]] = {
    import c.universe._

    val parameter = pattern.typeSymbol.isParameter

    val covariant = variance == Variance.Covariant &&
      (pattern.baseClasses contains scrutinee.typeSymbol)

    val contravariant = variance == Variance.Contravariant &&
      (scrutinee.baseClasses contains pattern.typeSymbol)

    val invariant = variance == Variance.Invariant &&
      (pattern.baseClasses contains scrutinee.typeSymbol) &&
      (scrutinee.baseClasses contains pattern.typeSymbol)

    if (parameter || covariant || contravariant || invariant) {
      val patternBase = if (covariant) pattern.baseType(scrutinee.typeSymbol) else pattern
      val scrutineeBase = if (contravariant) scrutinee.baseType(pattern.typeSymbol) else scrutinee

      val patternArgs = patternBase.typeArgs

      val scrutineeArgs = scrutineeBase match {
        case ExistentialType(quantified, underlying) =>
          underlying.substituteSymbols(quantified, List.fill(quantified.size)(NoSymbol)).typeArgs
        case _ =>
          scrutineeBase.typeArgs
      }

      if (parameter && patternArgs.isEmpty)
        Some(Map(pattern.typeSymbol -> scrutinee))
      else if (patternArgs.size == scrutineeArgs.size)
        ((patternArgs zip scrutineeArgs zip patternBase.typeConstructor.typeParams)
          .foldLeft[Option[Map[Symbol, Type]]](Some(Map.empty)) {
            case (instantiations, ((pattern, scrutinee), parameter)) =>
              if (scrutinee.typeSymbol != NoSymbol)
                instantiations flatMap { instantiations =>
                  typeInstantiations(c)(pattern, scrutinee, Variance(c)(parameter.asType)) map {
                    _ ++ instantiations
                  }
                }
              else
                instantiations
          }
          map { instantiations =>
            if (parameter)
              instantiations + (pattern.typeSymbol -> scrutinee.typeConstructor)
            else
              instantiations
          })
      else
        None
    }
    else
      None
  }

  private def importStatement(c: blackbox.Context)(path: List[(c.Symbol, c.Symbol)]) = {
    import c.universe._

    val meaningfulPath = path filter { case (stable, _) =>
      !stable.owner.isPackageClass || stable.name != typeNames.PACKAGE
    }

    meaningfulPath.size ->
      (meaningfulPath.reverseIterator map { case (stable, _) =>
        stable.name.decodedName.toString
      }).mkString("import ", ".", "._")
  }

  private def finalResultType(c: blackbox.Context)(symbol: c.Symbol, path: List[(c.Symbol, c.Symbol)]) =
    path.foldLeft(symbol.info.finalResultType) { case (tpe, (stable, owner)) =>
      val info = if (stable.isType) stable.asType.toType else stable.info.finalResultType
      tpe.asSeenFrom(info, owner)
    }

  private def relatedImportStatements(c: blackbox.Context)(
      implicitValues: List[(c.universe.MethodSymbol, List[(c.Symbol, c.Symbol)])],
      symbol: c.universe.MethodSymbol,
      tpe: c.Type,
      path: List[(c.Symbol, c.Symbol)]) = {
    import c.universe._

    val result = mutable.ListBuffer.empty[(Int, (Int, String))]
    val queue = mutable.Queue.empty[(MethodSymbol, Type, List[(Symbol, Symbol)], Int)]
    val visited = new util.HashSet[(MethodSymbol, List[(Symbol, Symbol)])]

    queue.enqueue((symbol, tpe, path, 0))

    while (queue.nonEmpty) {
      val (symbol, tpe, path, level) = queue.dequeue()

      if (visited.add(symbol -> path)) {
        val resultType = finalResultType(c)(symbol, path)

        typeInstantiations(c)(resultType, tpe, Variance.Covariant) foreach { instantiations =>
          val (from, to) = instantiations.toList.unzip

          symbol.paramLists.lastOption.toList foreach {
            _ foreach { arg =>
              val tpe = finalResultType(c)(arg, path).substituteTypes(from, to)

              if (isMeaningfulType(c)(tpe, tpe.typeSymbol))
                implicitValues foreach { case (symbol, path) =>
                  val (stable, _) = path.head
                  val resultType = symbol.info.finalResultType

                  if (isMeaningfulType(c)(resultType, resultType.typeSymbol))
                    typeInstantiations(c)(resultType, tpe, Variance.Covariant) foreach { _ =>
                      queue.enqueue((symbol, tpe, path, level + 1))

                      if (!(associatedImplicitScope(c)(tpe) contains stable))
                        result += level -> importStatement(c)(path)
                    }
                }
            }
          }
        }
      }
    }

    result.result()
  }

  private def hints(c: blackbox.Context)(tpe: c.Type, tpeIsBaseOfConversion: Boolean) = {
    import c.universe._

    noReporting(c.universe, List.empty[(List[(MethodSymbol, Type, List[(Symbol, Symbol)], (Int, String))], String, String)]) {
      val (implicitValues, implicitConversions) = findImplicits(c)(tpe, tpeIsBaseOfConversion)
      val scope = associatedImplicitScope(c)(tpe)

      val implicits =
        if (tpeIsBaseOfConversion)
          implicitConversions flatMap { case (symbol, path) =>
            val baseType = finalResultType(c)(symbol.paramLists.head.head, path)
            val resultType = finalResultType(c)(symbol, path)

            if (isMeaningfulType(c)(baseType, baseType.typeSymbol) &&
                isMeaningfulType(c)(resultType, resultType.typeSymbol))
              typeInstantiations(c)(baseType, tpe, Variance.Contravariant) map { instantiations =>
                val (from, to) = instantiations.toList.unzip
                (symbol, resultType.substituteTypes(from, to), path)
              }
            else
              None
          }
        else
          implicitValues flatMap { case (symbol, path) =>
            val resultType = finalResultType(c)(symbol, path)

            if (isMeaningfulType(c)(resultType, resultType.typeSymbol))
              typeInstantiations(c)(resultType, tpe, Variance.Covariant) map { instantiations =>
                val (from, to) = instantiations.toList.unzip
                (symbol, resultType.substituteTypes(from, to), path)
              }
            else
              None
          }

      val sortedEntries = ((implicits
        map { case (symbol, tpe, path) =>
          val (stable, _) = path.head
          (symbol,
           tpe,
           path,
           if (!(scope contains stable)) importStatement(c)(path) else 0 -> "")
        }
        groupBy { case (_, _, _, importStatement) => importStatement }).toList
        sortBy { case (importStatement, _) => importStatement })

      val selectedEntries =
        if (tpeIsBaseOfConversion)
          sortedEntries map { case entry @ (importStatement, entries) =>
            val typeCheckingEntries = entries filter { case (symbol, _, _, _) =>
              c.typecheck(q"$symbol(${c.prefix.tree})", silent = true).nonEmpty
            }

            if (typeCheckingEntries.nonEmpty)
              (importStatement, typeCheckingEntries)
            else
              entry
          }
        else
          sortedEntries

      selectedEntries map { case ((_, importStatement), entries) =>
        val importStatements =
          ((entries flatMap { case (symbol, tpe, path, _) =>
            relatedImportStatements(c)(implicitValues, symbol, tpe, path)
          }).sorted map { case (_, (_, importStatement)) =>
            importStatement
          }).distinct

        val related = importStatements filterNot { _ == importStatement }

        val relatedHint =
          if (related.nonEmpty) s"\n${related map { related => s"   (related `$related`)" } mkString "\n"}" else ""

        (entries, relatedHint, importStatement)
      }
    }
  }

  def extensions(c: blackbox.Context)(tpe: c.Type): String =
    if (!(c.settings contains "loci.macro.no-implicit-hints")) {
      val hint =
        (hints(c)(tpe, tpeIsBaseOfConversion = true)
          map { case (entries, relatedHint, importStatement) =>
            val identifiers = (entries flatMap { case (symbol, _, _, _) =>
              symbol.info.finalResultType.members collect {
                case symbol if isMeaningfulMethod(c)(symbol) =>
                  symbol.name.decodedName.toString
              }
            }).distinct.sorted

            if (identifiers.nonEmpty) {
              val importHint = if (importStatement.nonEmpty) s"\n   (via `$importStatement`)" else ""
              s"\n\nHint: You may use one of the following:\n   ${identifiers mkString ", "}$importHint$relatedHint"
            }
            else
              ""
          }).mkString

      if (hint.nonEmpty) s"$hint\n " else hint
    }
    else
      ""

  def values(c: blackbox.Context)(tpe: c.Type): String =
    if (!(c.settings contains "loci.macro.no-implicit-hints")) {
      val hint =
        (hints(c)(tpe, tpeIsBaseOfConversion = false)
          collect { case (_, relatedHint, importStatement) if importStatement.nonEmpty =>
            s"\n- `$importStatement`$relatedHint"
          }).mkString

      if (hint.nonEmpty) s"\n\nHint: You may consider the following imports:$hint\n " else hint
    }
    else
      ""
}
