package compiler.base

import clangast.types.*
import compiler.ext.CompileArray.getArrayRecordDecl
import compiler.ext.CompileProduct.getProductRecordDecl
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileType extends PartialCompiler {
  override def compileTypeRepr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case ConstantType(_: BooleanConstant) => CBoolType
        case tpe if tpe =:= TypeRepr.of[Boolean] => CBoolType
        case ConstantType(_: ByteConstant) => CCharType
        case tpe if tpe =:= TypeRepr.of[Byte] => CCharType
        case ConstantType(_: CharConstant) => CCharType
        case tpe if tpe =:= TypeRepr.of[Char] => CCharType
        case ConstantType(_: ShortConstant) => CShortType
        case tpe if tpe =:= TypeRepr.of[Short] => CShortType
        case ConstantType(_: IntConstant) => CIntegerType
        case tpe if tpe =:= TypeRepr.of[Int] => CIntegerType
        case ConstantType(_: LongConstant) => CLongType
        case tpe if tpe =:= TypeRepr.of[Long] => CLongType
        case ConstantType(_: FloatConstant) => CFloatType
        case tpe if tpe =:= TypeRepr.of[Float] => CFloatType
        case ConstantType(_: DoubleConstant) => CDoubleType
        case tpe if tpe =:= TypeRepr.of[Double] => CDoubleType
        case ConstantType(_: UnitConstant) => CVoidType
        case tpe if tpe =:= TypeRepr.of[Unit] => CVoidType
      }
    }

  def isNumberType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*

    tpe <:< TypeRepr.of[Byte] ||
      tpe <:< TypeRepr.of[Short] ||
      tpe <:< TypeRepr.of[Char] ||
      tpe <:< TypeRepr.of[Int] ||
      tpe <:< TypeRepr.of[Long] ||
      tpe <:< TypeRepr.of[Float] ||
      tpe <:< TypeRepr.of[Double]
  }

  def typeName(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect.*

    if (tpe <:< TypeRepr.of[Product] || tpe <:< TypeRepr.of[Array[?]]) {
      val className = tpe.classSymbol.get.name

      tpe.widen match {
        case AppliedType(_, typeArgs) =>
          val typeArgNames = typeArgs.map(typeName)
          className + "_" + typeArgNames.mkString("_")
        case _ => className
      }
    } else {
      tpe.typeSymbol.name
    }
  }

  def typeArgs(using Quotes): PartialFunction[quotes.reflect.TypeRepr, List[quotes.reflect.TypeRepr]] = tpe => {
    import quotes.reflect.*

    tpe match {
      case MethodType(_, _, AppliedType(_, typeArgs)) => typeArgs
      case AppliedType(_, typeArgs) => typeArgs
      case _ => throw new MatchError(tpe.show(using Printer.TypeReprStructure))
    }
  }
}
