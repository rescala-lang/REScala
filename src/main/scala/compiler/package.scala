import compiler.base.*
import compiler.context.*
import compiler.debug.CompileDebug
import compiler.ext.*

package object compiler {
  val minimalCascade: CompilerCascade = CompilerCascade(
    CompileDebug,
    CompileDataStructure,
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

  trait MinimalContext extends ValueDeclTC with FunctionDeclTC with RecordDeclTC

  def createMinimalContext(): MinimalContext = new MinimalContext {}

  val standardCascade: CompilerCascade =
    CompileMainFunction
      ~>: CompileHelperFun
      ~>: CompileMap
      ~>: CompileEither
      ~>: CompileOption
      ~>: CompileArray
      ~>: CompileProduct
      ~>: CompileString
      ~>: minimalCascade

  trait StandardContext extends MinimalContext

  def createStandardContext(): StandardContext = new StandardContext {}
}
