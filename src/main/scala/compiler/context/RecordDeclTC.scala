package compiler.context

import clangast.decl.{CFunctionDecl, CRecordDecl}

import scala.collection.mutable

trait RecordDeclTC extends TranslationContext {
  val nameToRecordDecl: MappingLabel[String, CRecordDecl] = MappingLabel(typeDecls.append)

  val nameToRecordCreator: MappingLabel[String, CFunctionDecl] = MappingLabel(valueDecls.append)

  val nameToRecordEquals: MappingLabel[String, CFunctionDecl] = MappingLabel(valueDecls.append)
  
  val nameToRecordPrinter: MappingLabel[String, CFunctionDecl] = MappingLabel(valueDecls.append)
}
