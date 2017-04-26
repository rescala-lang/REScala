package rescala.fullmv

import rescala.engine.{EngineImpl, Turn}
import rescala.graph.{Pulsing, Reactive, Struct}

trait FullMVStruct extends Struct {
  override type  State[P, S <: Struct] = NodeVersionHistory[P, S]
}

class FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn
  override protected def executeTurn[R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: (FullMVTurn) => R): R = ???
}

class FullMVTurn extends Turn[FullMVStruct] {
  override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[FullMVStruct]], hasState: Boolean) = new NodeVersionHistory[P, FullMVStruct]()

  /**
    * Synchronize for access (i.e., [[before()]] or [[after()]]) on this node when
    * synchronization is unknown. Multiple invocations are redundant, but not harmful outside of an
    * implementation-dependent performance penalty.
    *
    * @param reactive the reactive to be dynamically accessed
    */
  override private[rescala] def dynamicDependencyInteraction(reactive: Reactive[FullMVStruct]) = ???

  /**
    * Read value from before this turn. Only call this if you know that you are synchronized with this node:
    * Reads on dependencies where an edge exists (e.g., reading a static dependency) is always synchronized.
    * Reads on other nodes must be synchronized through [[dynamicDependencyInteraction()]] first.
    *
    * @param pulsing the node to be read
    * @tparam P the node's storage type
    * @return the stored value from before this turn
    */
  override private[rescala] def before[P](pulsing: Pulsing[P, FullMVStruct]) = ???

  /**
    * Read value from after this turn. Implementations may return the node's current value, including
    * changes already made by this turn but disregarding potential future changes, or may suspend to
    * return only the value that is final until the end of this turn.
    * Only call this if you know that you are synchronized with this node:
    * Reads on dependencies where an edge exists (e.g., reading a static dependency) is always synchronized.
    * Reads on other nodes must be synchronized through [[dynamicDependencyInteraction()]] first.
    *
    * @param pulsing the node to be read
    * @tparam P the node's storage type
    * @return the stored value from after this turn
    */
override private[rescala] def after[P](pulsing: Pulsing[P, FullMVStruct]) = ???

  /**
    * Connects a reactive element with potentially existing dependencies and prepares re-evaluations to be
    * propagated based on the turn's propagation scheme
    *
    * @param dependencies Existing reactive elements the element depends on
    * @param dynamic      Indicates if the element uses dynamic re-evaluation to determine it's dependencies
    * @param f            Reactive element to prepare and connect
    * @tparam T Reactive subtype of the reactive element
    * @return Connected reactive element
    */
override private[rescala] def create[T <: Reactive[FullMVStruct]](dependencies: Set[Reactive[FullMVStruct]], dynamic: Boolean)(f: => T) = ???

  /**
    * Registers a new handler function that is called after all changes were written and committed.
    *
    * @param f Handler function to register.
    */
  override def observe(f: () => Unit): Unit = ???
}
