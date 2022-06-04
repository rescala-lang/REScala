package compiler

import clangast.decl.{CFunctionDecl, CInclude, CRecordDecl, CValueDecl}

import scala.collection.mutable

class TranslationContext {
  val includes: mutable.Set[CInclude] = mutable.Set()
  
  val nameToDecl: mutable.Map[String, CValueDecl] = mutable.Map()
  
  val nameToRecordDecl: mutable.Map[String, CRecordDecl] = mutable.Map()
  
  val nameToFunctionDecl: mutable.Map[String, CFunctionDecl] = mutable.Map()
  
  val nameToRecordCreator: mutable.Map[String, CFunctionDecl] = mutable.Map()
  
  val nameToRecordEquals: mutable.Map[String, CFunctionDecl] = mutable.Map()
}
