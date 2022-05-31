package rescala.meta
import rescala.core.ReName
import rescala.macros.MacroAccess
import rescala.operator.RExceptions

class MetaBundle extends rescala.core.Core {
  bundle =>

  override type State[A] = MReSource

  trait MReSource extends ReSource {
    def triggers: Seq[ReSource]
    type Value = Unit
    override protected[rescala] def commit(base: Value): Value = base
  }

  case object Tx extends Transaction {

    private[rescala] def access(reactive: ReSource): reactive.Value = ???

    def initializer: Initializer = new Initializer:
      override protected[this] def makeDerivedStructState[V](initialValue: V): State[V] = ???
      override protected[this] def initialize(reactive: Derived, incoming: Set[ReSource], needsReevaluation: Boolean): Unit = ()
  }

  given (using ReName): CreationTicket = CreationTicket(new ScopeSearch(Left(Tx)), summon)

  case class Signal[T](triggers: Seq[MReSource], name: ReName) extends MReSource with MacroAccess[T, MReSource] with ReadAs[T] {
    override protected[rescala] def state: State[T] = this
    override def resource: MReSource = this
    override def read(v: Unit): T = ???
  }

  object Signal {
    inline def apply[T](inline expr: T)(implicit ct: CreationTicket): Signal[T] = ${
      rescala.macros.reactiveMacro[T, [x] =>> x, bundle.type, Signal]('expr, 'bundle, 'ct, '{ "Signal" }, 'true)
    }
  }

  object Signals {
    def staticNoVarargs[T](dependencies: Seq[ReSource])(expr: StaticTicket => T)(using ct: CreationTicket): Signal[T] =
      Signal(dependencies.map(_.state), ct.rename)
  }



}
