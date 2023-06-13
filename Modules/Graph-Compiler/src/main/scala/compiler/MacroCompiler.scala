package compiler

import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType
import clangast.{CASTNode, WithContext}


trait MacroCompiler {
  inline def compileTree(inline t: Any): WithContext[CASTNode]

  inline def compileExpr(inline e: Any): WithContext[CExpr]

  inline def compileFun(inline f: AnyRef): WithContext[CFunctionDecl]

  inline def compileAnonFun(inline f: AnyRef): WithContext[CFunctionDecl]

  inline def compileType[T]: WithContext[CType]

  inline def valName: String
}
