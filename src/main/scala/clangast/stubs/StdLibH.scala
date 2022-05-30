package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import clangast.types.CVoidType

object StdLibH {
  val include: CInclude = CInclude("stdlib.h")
  
  val atof: CFunctionDecl = CFunctionStub("atof")
  val atoi: CFunctionDecl = CFunctionStub("atoi")
  val atol: CFunctionDecl = CFunctionStub("atol")
  val strtod: CFunctionDecl = CFunctionStub("strtod")
  val strtol: CFunctionDecl = CFunctionStub("strtol")
  val strtoul: CFunctionDecl = CFunctionStub("strtoul")
  val calloc: CFunctionDecl = CFunctionStub("calloc")
  val free: CFunctionDecl = CFunctionStub("free")
  val malloc: CFunctionDecl = CFunctionStub("malloc")
  val realloc: CFunctionDecl = CFunctionStub("realloc")
  val abort: CFunctionDecl = CFunctionStub("abort")
  val atexit: CFunctionDecl = CFunctionStub("atexit")
  val exit: CFunctionDecl = CFunctionStub("exit")
  val getenv: CFunctionDecl = CFunctionStub("getenv")
  val system: CFunctionDecl = CFunctionStub("system")
  val bsearch: CFunctionDecl = CFunctionStub("bsearch")
  val qsort: CFunctionDecl = CFunctionStub("qsort")
  val abs: CFunctionDecl = CFunctionStub("abs")
  val div: CFunctionDecl = CFunctionStub("div")
  val labs: CFunctionDecl = CFunctionStub("labs")
  val ldiv: CFunctionDecl = CFunctionStub("ldiv")
  val rand: CFunctionDecl = CFunctionStub("rand")
  val srand: CFunctionDecl = CFunctionStub("srand")
  val mblen: CFunctionDecl = CFunctionStub("mblen")
  val mbstowcs: CFunctionDecl = CFunctionStub("mbstowcs")
  val mbtowc: CFunctionDecl = CFunctionStub("mbtowc")
  val wcstombs: CFunctionDecl = CFunctionStub("wcstombs")
  val wctomb: CFunctionDecl = CFunctionStub("wctomb")
}
