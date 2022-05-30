package macros

import clangast.given
import clangast.*
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAssignmentExpr, CLessThanExpr, CNotEqualsExpr}
import clangast.expr.unaryop.{CIncExpr, CNotExpr}
import clangast.stmt.*
import clangast.stubs.{StdArgH, StdLibH}
import clangast.types.*

import CompileProduct.getProductRecordDecl
import CompileArray.getArrayRecordDecl

import scala.quoted.*

object CompileType {
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

  def compileTypeRepr(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CType = {
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Boolean] then CBoolType
    else if tpe =:= TypeRepr.of[Byte] then CCharType
    else if tpe =:= TypeRepr.of[Char] then CCharType
    else if tpe =:= TypeRepr.of[Short] then CShortType
    else if tpe =:= TypeRepr.of[Int] then CIntegerType
    else if tpe =:= TypeRepr.of[Long] then CLongType
    else if tpe =:= TypeRepr.of[Float] then CFloatType
    else if tpe =:= TypeRepr.of[Double] then CDoubleType
    else if tpe =:= TypeRepr.of[Unit] then CVoidType
    else if tpe <:< TypeRepr.of[Product] then CRecordType(getProductRecordDecl(tpe, ctx))
    else if tpe <:< TypeRepr.of[Array[?]] then CRecordType(getArrayRecordDecl(tpe, ctx))
    else throw new MatchError(tpe.show(using Printer.TypeReprStructure))
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
}
