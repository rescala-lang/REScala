package loci
package utility

import scala.annotation.targetName
import scala.quoted.*
import scala.util.control.NonFatal

object reflectionExtensions:
  extension (using Quotes)(symbol: quotes.reflect.Symbol)
    def isAbstract =
      (symbol.flags is quotes.reflect.Flags.Abstract) ||
      (symbol.flags is quotes.reflect.Flags.Deferred)

    def isMethod =
      symbol.isTerm &&
      !symbol.isClassConstructor &&
      (symbol.flags is quotes.reflect.Flags.Method)

    def isField =
      symbol.isTerm &&
      !(symbol.flags is quotes.reflect.Flags.Method) &&
      !(symbol.flags is quotes.reflect.Flags.Module) &&
      !(symbol.flags is quotes.reflect.Flags.Package)

    def isStable =
      symbol.isTerm &&
      ((!(symbol.flags is quotes.reflect.Flags.Mutable) &&
        !(symbol.flags is quotes.reflect.Flags.Method)) ||
       (symbol.flags is quotes.reflect.Flags.StableRealizable))

    def isPublic =
      !(symbol.flags is quotes.reflect.Flags.Protected) &&
      !(symbol.flags is quotes.reflect.Flags.Private) &&
      !(symbol.flags is quotes.reflect.Flags.PrivateLocal) &&
      !(symbol.flags is quotes.reflect.Flags.Local)

    def isImplicit =
      (symbol.flags is quotes.reflect.Flags.Implicit) ||
      (symbol.flags is quotes.reflect.Flags.Given)

    def isExtensionMethod =
      symbol.flags is quotes.reflect.Flags.ExtensionMethod

    def isModuleDef =
      symbol.flags is quotes.reflect.Flags.Module
  end extension

  extension (using Quotes)(tree: quotes.reflect.Tree)
    @targetName("safeShowTree") def safeShow: String = tree.safeShow("<?>", quotes.reflect.Printer.TreeCode)
    @targetName("safeShowTree") def safeShow(fallback: String): String = tree.safeShow(fallback, quotes.reflect.Printer.TreeCode)
    @targetName("safeShowTree") def safeShow(fallback: String, printer: quotes.reflect.Printer[quotes.reflect.Tree]): String =
      try
        val result = tree.show(using printer).trim
        if result.nonEmpty then result else fallback
      catch
        case NonFatal(_) => fallback
  end extension

  extension (using Quotes)(tpe: quotes.reflect.TypeRepr)
    @targetName("safeShowType") def safeShow: String = tpe.safeShow("<?>", quotes.reflect.Printer.SafeTypeReprCode)
    @targetName("safeShowType") def safeShow(fallback: String): String = tpe.safeShow(fallback, quotes.reflect.Printer.SafeTypeReprCode)
    @targetName("safeShowType") def safeShow(fallback: String, printer: quotes.reflect.Printer[quotes.reflect.TypeRepr]): String =
      try
        val result = tpe.show(using printer).trim
        if result.nonEmpty then result else fallback
      catch
        case NonFatal(_) => fallback

    def typeConstructor: quotes.reflect.TypeRepr =
      tpe match
        case quotes.reflect.AppliedType(tycon, _) => tycon
        case _ => tpe

    def typeArgs: List[quotes.reflect.TypeRepr] =
      tpe match
        case quotes.reflect.AppliedType(_, args) => args
        case _ => List.empty

    def typeArgVariances: List[quotes.reflect.Flags] =
      tpe match
        case TypeArgVariances(variances) => variances
        case _ => List.empty

    def resultType: quotes.reflect.TypeRepr =
      tpe match
        case tpe: quotes.reflect.MethodOrPoly => tpe.resType.resultType
        case _ => tpe

    def contextFunctionResultType: quotes.reflect.TypeRepr =
      if tpe.isContextFunctionType then
        tpe.typeArgs.lastOption map { _.contextFunctionResultType } getOrElse tpe
      else
        tpe

    def finalResultType: quotes.reflect.TypeRepr =
      tpe.resultType.contextFunctionResultType

    def substitute(from: quotes.reflect.ParamRef, to: quotes.reflect.TypeRepr) =
      TypeParamSubstition.substitute(tpe, from, to)

    def resolvedMemberType(symbol: quotes.reflect.Symbol) =
      import quotes.reflect.*

      def memberTypeInRefinement(tpe: TypeRepr, name: String): Option[TypeRepr] = tpe match
        case Refinement(_, `name`, info) => Some(info)
        case Refinement(parent, _, _) => memberTypeInRefinement(parent, name)
        case _ => None

      memberTypeInRefinement(tpe, symbol.name) orElse
      memberTypeInRefinement(tpe.dealias, symbol.name) getOrElse
      tpe.memberType(tpe.baseClasses
        collectFirst Function.unlift { base =>
          val overriding = symbol.overridingSymbol(base)
          Option.when(overriding.exists)(overriding)
        }
        getOrElse symbol)
  end extension

  trait SimpleTypeMap[Q <: Quotes & Singleton](val quotes: Q):
    import quotes.reflect.*

    def transform(tpe: TypeRepr): TypeRepr = tpe match
      case tpe: AppliedType =>
        val tycon = transform(tpe.tycon)
        val args = tpe.args map transform
        if tycon != tpe.tycon || args != tpe.args then tycon.appliedTo(args) else tpe
      case tpe: TypeBounds =>
        val low = transform(tpe.low)
        val hi = transform(tpe.hi)
        if low != tpe.low || hi != tpe.hi then TypeBounds(low, hi) else tpe
      case tpe: Refinement =>
        val parent = transform(tpe.parent)
        val info = transform(tpe.info)
        if parent != tpe.parent || info != tpe.info then Refinement(parent, tpe.name, info) else tpe
      case _ =>
        tpe
  end SimpleTypeMap

  extension (using Quotes)(printerModule: quotes.reflect.PrinterModule)
    def SafeTypeReprCode = new quotes.reflect.Printer[quotes.reflect.TypeRepr]:
      import quotes.reflect.*

      def show(tpe: TypeRepr) = showType(tpe)

      private def showType(tpe: TypeRepr): String =
        try tpe.show
        catch case NonFatal(_) =>
          try tpe match
            case AppliedType(tycon, args) =>
              val tyconName = showType(tycon)
              val argsNames = args map showType
              if tyconName.isEmpty then ""
              else if argsNames contains "" then tyconName
              else s"$tyconName[${argsNames.mkString(", ")}]"
            case tpe: NamedType if tpe.typeSymbol != defn.RootClass =>
              val owner = tpe.typeSymbol.owner
              val qualifierName = showType(tpe.qualifier)
              if qualifierName.nonEmpty then s"$qualifierName.${tpe.name}"
              else if owner != defn.RootClass then s"${owner.fullName}.${tpe.name}"
              else tpe.name
            case AnnotatedType(underlying, annotation) =>
              val underlyingName = showType(underlying)
              val annotationName =
                try annotation.show.stripPrefix("new ")
                catch case NonFatal(_) => ""
              if annotationName.nonEmpty then s"$underlyingName @$annotationName"
              else underlyingName
            case MatchType(_, scrutinee, cases) =>
              val casesNames = cases map showType
              s"${showType(scrutinee)} match { ${casesNames.mkString(" ")} }"
            case MatchCase(pattern, rhs) =>
              s"case ${showType(pattern)} => ${showType(rhs)}"
            case ThisType(ref) if ref.typeSymbol.isPackageDef || ref.typeSymbol.isModuleDef =>
              showType(ref)
            case ThisType(ref) =>
              s"${showType(ref)}.this"
            case SuperType(ThisType(ref), _) =>
              s"${showType(ref)}.super"
            case SuperType(ref, _) =>
              s"${showType(ref)}.super"
            case Refinement(parent: Refinement, name, info) =>
              s"${showType(parent).stripSuffix(" }")}; ${showRefinement(name, info)} }"
            case Refinement(parent, name, info) =>
              s"${showType(parent)} { ${showRefinement(name, info)} }"
            case ByNameType(underlying) =>
              s"=> ${showType(underlying)}"
            case tpe @ MethodType(_, _, _: MethodOrPoly) =>
              showLambdaType(tpe, "(", ":", ")")
            case tpe: MethodType =>
              showLambdaType(tpe, "(", ":", "): ")
            case tpe @ PolyType(_, _, _: MethodOrPoly) =>
              showLambdaType(tpe, "[", "", "]")
            case tpe: PolyType =>
              showLambdaType(tpe, "[", "", "]: ")
            case tpe: TypeLambda =>
              showLambdaType(tpe, "[", "", "] =>> ")
            case ParamRef(binder: LambdaType, paramNum) =>
              binder.paramNames(paramNum)
            case tpe: AndType =>
              showAndOrType(tpe, "&")
            case tpe: OrType =>
              showAndOrType(tpe, "|")
            case TypeBounds(low, hi) =>
              s">: ${showType(low)} <: ${showType(hi)}"
            case ConstantType(constant) =>
              constant.value.toString
            case _ =>
              ""
          catch case NonFatal(_) => ""

      private def showLambdaType(tpe: LambdaType, open: String, mid: String, close: String) =
        val args = tpe.paramNames zip tpe.paramTypes map { (name, tpe) => s"$name$mid ${showType(tpe)}" }
        s"${args.mkString(open, ", ", close)}${showType(tpe.resType)}"

      private def showAndOrType(tpe: AndOrType, operator: String) =
        val leftName = showType(tpe.left)
        val rightName = showType(tpe.right)
        if leftName.nonEmpty && rightName.nonEmpty then s"($leftName $operator $rightName)"
        else if leftName.nonEmpty then leftName
        else if rightName.nonEmpty then rightName
        else ""

      private def showRefinement(name: String, info: TypeRepr) = info match
        case _: TypeBounds =>
          s"type $name ${showType(info)}"
        case ByNameType(underlying) =>
          s"def $name: ${showType(underlying)}"
        case _: MethodOrPoly =>
          s"def $name${showType(info)}"
        case _ =>
          s"val $name: ${showType(info)}"
    end SafeTypeReprCode
  end extension

  private object TypeParamSubstition:
    private val substituteTypeParam =
      try
        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
        val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")
        val paramRefClass = Class.forName("dotty.tools.dotc.core.Types$ParamRef")

        val ctx = quotesImplClass.getMethod("ctx")
        val substParam = typeClass.getMethod("substParam", paramRefClass, typeClass, contextClass)

        { (quotes: Quotes, tpe: Any, from: Any, to: Any) =>
          import quotes.reflect.*

          try
            if paramRefClass.isInstance(from) && typeClass.isInstance(to) && typeClass.isInstance(tpe) then
              substParam.invoke(tpe, from, to, ctx.invoke(quotes)) match
                case tpe: TypeRepr @unchecked if typeClass.isInstance(tpe) =>
                  Some(tpe)
                case _ =>
                  None
            else
              None
          catch { case _: IllegalArgumentException | _: ClassCastException => None }
        }
      catch
        case _: ClassNotFoundException | _: NoSuchMethodException =>
          (quotes: Quotes, tpe: Any, from: Any, to: Any) => None

    def substitute(using Quotes)(tpe: quotes.reflect.TypeRepr, from: quotes.reflect.ParamRef, to: quotes.reflect.TypeRepr) =
      substituteTypeParam(quotes, tpe, from, to) getOrElse tpe
  end TypeParamSubstition

  private object TypeArgVariances:
    private def varianceFlags(using Quotes)(flags: quotes.reflect.Flags) =
      import quotes.reflect.*
      if (flags is Flags.Covariant) && !(flags is Flags.Contravariant) then Flags.Covariant
      else if (flags is Flags.Contravariant) && !(flags is Flags.Covariant) then Flags.Contravariant
      else Flags.EmptyFlags

    private def variancesOfSymbol(using Quotes)(symbol: quotes.reflect.Symbol) =
      Some(symbol.declaredTypes collect { case symbol if symbol.isTypeParam => varianceFlags(symbol.flags) })

    private val variancesOfType =
      try
        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
        val typeLambdaClass = Class.forName("dotty.tools.dotc.core.Types$TypeLambda")
        val lambdaParamClass = Class.forName("dotty.tools.dotc.core.Types$LambdaParam")

        val ctx = quotesImplClass.getMethod("ctx")
        val typeParams = typeLambdaClass.getMethod("typeParams")
        val paramVariance = lambdaParamClass.getMethod("paramVariance", contextClass)

        { (quotes: Quotes, tpe: Any) =>
          import quotes.reflect.*

          try
            if typeLambdaClass.isInstance(tpe) then
              typeParams.invoke(tpe) match
                case params: List[_] =>
                  val flagsClass = Flags.EmptyFlags.getClass
                  val context = ctx.invoke(quotes)
                  val variances = params map { param =>
                    paramVariance.invoke(param, context) match
                      case flags: Flags @unchecked if flagsClass.isInstance(flags) =>
                        Some(varianceFlags(using quotes)(flags))
                      case _ =>
                        None
                  }

                  if variances contains None then None
                  else Some(variances.flatten)

                case _ =>
                  None
            else
              None
          catch { case _: IllegalArgumentException | _: ClassCastException => None }
        }
      catch
        case _: ClassNotFoundException | _: NoSuchMethodException =>
          (quotes: Quotes, tpe: Any) => None

    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      import quotes.reflect.*

      tpe match
        case tpe: LambdaType => variancesOfType(quotes, tpe)
        case TypeBounds(_, tpe: LambdaType) => variancesOfType(quotes, tpe)
        case ParamRef(PolyType(_, bounds, _), paramNum) =>
          bounds(paramNum).hi match
            case tpe: LambdaType => variancesOfType(quotes, tpe)
            case _ => variancesOfSymbol(tpe.typeSymbol)
        case _ => variancesOfSymbol(tpe.typeSymbol)
    end unapply
  end TypeArgVariances
end reflectionExtensions
