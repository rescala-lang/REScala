package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}

object MathH {
  val include: CInclude = CInclude("math.h")
  
  val acos: CFunctionDecl = CFunctionStub("acos")
  val asin: CFunctionDecl = CFunctionStub("asin")
  val atan: CFunctionDecl = CFunctionStub("atan")
  val atan2: CFunctionDecl = CFunctionStub("atan2")
  val cos: CFunctionDecl = CFunctionStub("cos")
  val cosh: CFunctionDecl = CFunctionStub("cosh")
  val sin: CFunctionDecl = CFunctionStub("sin")
  val sinh: CFunctionDecl = CFunctionStub("sinh")
  val tanh: CFunctionDecl = CFunctionStub("tanh")
  val exp: CFunctionDecl = CFunctionStub("exp")
  val frexp: CFunctionDecl = CFunctionStub("frexp")
  val ldexp: CFunctionDecl = CFunctionStub("ldexp")
  val log: CFunctionDecl = CFunctionStub("log")
  val log10: CFunctionDecl = CFunctionStub("log10")
  val modf: CFunctionDecl = CFunctionStub("modf")
  val pow: CFunctionDecl = CFunctionStub("pow")
  val sqrt: CFunctionDecl = CFunctionStub("sqrt")
  val ceil: CFunctionDecl = CFunctionStub("ceil")
  val fabs: CFunctionDecl = CFunctionStub("fabs")
  val floor: CFunctionDecl = CFunctionStub("floor")
  val fmod: CFunctionDecl = CFunctionStub("fmod")
}
