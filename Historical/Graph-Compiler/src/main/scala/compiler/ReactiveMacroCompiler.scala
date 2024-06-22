package compiler

trait ReactiveMacroCompiler extends MacroCompiler {
  inline def compileGraph(inline appName: String)(inline graph: Any): Unit
}
