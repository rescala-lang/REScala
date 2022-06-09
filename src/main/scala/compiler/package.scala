import compiler.base.*
import compiler.context.*
import compiler.ext.*

package object compiler {
  val minimalCascade: CompilerCascade = CompilerCascade(
    CompileApply,
    CompileDefinition,
    CompileMatch,
    CompileRef,
    CompileSelect,
    CompileStatement,
    CompileTerm,
    CompileTree,
    CompileType
  )

  trait MinimalContext extends IncludeTC with ValueDeclTC with FunctionDeclTC with RecordDeclTC

  def createMinimalContext(): MinimalContext = new MinimalContext {}

  val standardCascade: CompilerCascade =
    CompileMainFunction
      ~>: CompileHelperFun
      ~>: CompileArray
      ~>: CompileProduct
      ~>: CompileString
      ~>: minimalCascade

  trait StandardContext extends MinimalContext

  def createStandardContext(): StandardContext = new StandardContext {}
}
