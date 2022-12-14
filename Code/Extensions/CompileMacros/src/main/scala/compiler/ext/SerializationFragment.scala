package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.CFunctionDecl
import clangast.expr.{CCallExpr, CCastExpr, CExpr, CParenExpr}
import clangast.stubs.CJSONH
import clangast.types.{CCharType, CFloatType, CShortType}
import compiler.base.*
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.context.TranslationContext

import scala.quoted.*

object SerializationFragment extends SerializationIFFragment {
  override def serializationRetainsEquality(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Boolean | Char | Byte | Short | Int | Long | String] => true
      case _                                                                               => false
    }
  }

  def serialize(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      TranslationContext
  ): CExpr = {
    import quotes.reflect.*

    tpe match {
      case _ if tpe <:< TypeRepr.of[Boolean] =>
        CCallExpr(CJSONH.cJSON_CreateBool.ref, List(expr))
      case _ if tpe <:< TypeRepr.of[Byte | Char | Short | Int | Long | Float | Double] =>
        CCallExpr(CJSONH.cJSON_CreateNumber.ref, List(expr))
      case _ if tpe <:< TypeRepr.of[String] =>
        CCallExpr(CJSONH.cJSON_CreateString.ref, List(expr))
      case _ => CCallExpr(dispatch[SerializationIFFragment](_.compileSerialize)(tpe).ref, List(expr))
    }
  }

  def deserialize(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      TranslationContext
  ): CExpr = {
    import quotes.reflect.*

    tpe match {
      case _ if tpe <:< TypeRepr.of[Boolean] =>
        CCallExpr(CJSONH.cJSON_IsTrue.ref, List(expr))
      case _ if tpe <:< TypeRepr.of[Int | Long] =>
        CJSONH.valueint(expr)
      case _ if tpe <:< TypeRepr.of[Byte | Char] =>
        CParenExpr(CCastExpr(CJSONH.valueint(expr), CCharType))
      case _ if tpe <:< TypeRepr.of[Short] =>
        CParenExpr(CCastExpr(CJSONH.valueint(expr), CShortType))
      case _ if tpe <:< TypeRepr.of[Double] =>
        CJSONH.valuedouble(expr)
      case _ if tpe <:< TypeRepr.of[Float] =>
        CParenExpr(CCastExpr(CJSONH.valuedouble(expr), CFloatType))
      case _ => CCallExpr(dispatch[SerializationIFFragment](_.compileDeserialize)(tpe).ref, List(expr))
    }
  }
}
