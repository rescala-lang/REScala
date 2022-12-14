package compiler.context

import clangast.decl.{CFunctionDecl, CRecordDecl}

import scala.collection.mutable

trait RecordDeclTC extends TranslationContext {
  val nameToRecordDecl: MappingLabel[String, CRecordDecl] = MappingLabel(typeDecls.append)

  val recordFunMap: MappingLabel[(String, String), CFunctionDecl] = MappingLabel(valueDecls.append)
}
