package clangast.stubs

import clangast.given
import clangast.decl.{CFunctionDecl, CInclude, CTypedefDecl}
import clangast.types.CVoidType

object StdArgH {
  val include: CInclude = CInclude("stdarg.h")
  
  val va_list: CTypedefDecl = CTypedefDecl("va_list", CVoidType)
  
  val va_start: CFunctionDecl = CFunctionStub("va_start")
  val va_arg: CFunctionDecl = CFunctionStub("va_arg")
  val va_end: CFunctionDecl = CFunctionStub("va_end")
}
