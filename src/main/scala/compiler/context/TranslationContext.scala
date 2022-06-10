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
}
