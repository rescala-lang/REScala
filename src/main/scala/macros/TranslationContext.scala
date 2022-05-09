package macros

import clangast.decl.{CFunctionDecl, CRecordDecl, CValueDecl}

import scala.collection.mutable

class TranslationContext {
  val nameToDecl: mutable.Map[String, CValueDecl] = mutable.Map()
  
  val nameToRecordDecl: mutable.Map[String, CRecordDecl] = mutable.Map()
  
  val nameToRecordCreator: mutable.Map[String, CFunctionDecl] = mutable.Map()
  
  val nameToRecordEquals: mutable.Map[String, CFunctionDecl] = mutable.Map()
}
