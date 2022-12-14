import compiler.base.*
import compiler.context.*
import compiler.debug.DebugFragment
import compiler.ext.*

package object compiler {
  val minimalFragmentedCompiler: FragmentedCompiler = FragmentedCompiler(
    DebugFragment,
    DataStructureFragment,
    ApplyFragment,
    DefinitionFragment,
    MatchFragment,
    RefFragment,
    SelectFragment,
    StatementFragment,
    TermFragment,
    TreeFragment,
    TypeFragment
  )

  trait MinimalContext extends ValueDeclTC with FunctionDeclTC with RecordDeclTC

  def createMinimalContext(): MinimalContext = new MinimalContext {}

  val standardFragmentedCompiler: FragmentedCompiler =
    MainFunctionFragment
    +: HelperFunFragment
    +: SetFragment
    +: MapFragment
    +: EitherFragment
    +: OptionFragment
    +: ArrayFragment
    +: ProductFragment
    +: SerializationFragment
    +: StringFragment
    +: minimalFragmentedCompiler

  trait StandardContext extends MinimalContext

  def createStandardContext(): StandardContext = new StandardContext {}
}
