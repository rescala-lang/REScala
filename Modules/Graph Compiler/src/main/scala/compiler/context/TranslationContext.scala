package compiler.context

import clangast.decl.{CInclude, CTypeDecl, CValueDecl}

import scala.collection.mutable

trait TranslationContext {
  protected val includes: mutable.Set[CInclude] = mutable.Set()

  def addInclude(include: CInclude): Unit = includes.add(include)

  def includesList: List[CInclude] = includes.toList

  protected val typeDecls: mutable.ListBuffer[CTypeDecl] = mutable.ListBuffer()

  def addTypeDecl(typeDecl: CTypeDecl): Unit = typeDecls.append(typeDecl)

  def typeDeclList: List[CTypeDecl] = typeDecls.toList

  protected val valueDecls: mutable.ListBuffer[CValueDecl] = mutable.ListBuffer()

  def valueDeclList: List[CValueDecl] = valueDecls.toList

  def addValueDecl(valueDecl: CValueDecl): Unit = valueDecls.append(valueDecl)

  protected val valueNames: mutable.Map[String, Int] = mutable.Map()

  def registerValueName(name: String): Unit = valueNames.updateWith(name) {
    case None    => Some(1)
    case Some(n) => Some(n + 1)
  }

  def uniqueValueName(from: String): String = {
    registerValueName(from)

    valueNames(from) match {
      case 1 => from
      case n => from + "_" + n
    }
  }
}
