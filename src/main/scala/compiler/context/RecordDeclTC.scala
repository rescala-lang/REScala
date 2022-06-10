package compiler.context

import clangast.decl.{CFunctionDecl, CRecordDecl}

import scala.collection.mutable

trait RecordDeclTC extends TranslationContext {
  val nameToRecordDecl: mutable.Map[String, CRecordDecl] = mutable.Map()

  val orderedRecordDecls: mutable.ListBuffer[CRecordDecl] = mutable.ListBuffer()

  def getOrElseUpdateRecordDecl(key: String, updated: => CRecordDecl): CRecordDecl = {
    nameToRecordDecl.get(key) match {
      case Some(decl) => decl
      case None =>
        nameToRecordDecl.put(key, updated)
        orderedRecordDecls.append(updated)
        updated
    }
  }

  val nameToRecordCreator: mutable.Map[String, CFunctionDecl] = mutable.Map()

  val nameToRecordEquals: mutable.Map[String, CFunctionDecl] = mutable.Map()
  
  val nameToRecordPrinter: mutable.Map[String, CFunctionDecl] = mutable.Map()
}
