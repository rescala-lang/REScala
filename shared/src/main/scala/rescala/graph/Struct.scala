package rescala.graph

import rescala.propagation.{Committable, Turn}

import scala.language.{existentials, higherKinds, implicitConversions}

/**
  * Buffer companion object. Only used for pipelining.
  */
object Buffer {
  type CommitStrategy[A] = (A, A) => A
  def commitAsIs[A](base: A, cur: A): A = cur
  def transactionLocal[A](base: A, cur: A) = base
  def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = cur.keep
}

/**
  * Buffer that stores a temporary new value for its content until it is committed.
  *
  * @tparam A Buffer stored content type
  */
// TODO: Does a non-committable buffer make sense? Maybe make it extend Committable directly
trait Buffer[A] {
  def transform(f: (A) => A)(implicit turn: Turn[_]): A
  def set(value: A)(implicit turn: Turn[_]): Unit
  def base(implicit turn: Turn[_]): A
  def get(implicit turn: Turn[_]): A
}

/**
  * Wrapper that adds a level of indirection for classes having a spore type dependency.
 */
trait Struct {
  /**
    * Spore type defined by this struct with inferred pulse value type
    *
    * @tparam R Reactive value type represented by the spore
    */
  final type Spore[R] = SporeP[_, R]

  /**
    * Spore type defined by this struct
    *
    * @tparam P Pulse stored value type
    * @tparam R Reactive value type represented by the spore
    */
  type SporeP[P, R]
}

/**
  * Wrapper for a spore type combining GraphSpore and PulsingSpore
  */
trait PulsingGraphStruct extends Struct {
  override type SporeP[P, R] <: GraphSpore[R] with PulsingSpore[P]
}

/**
  * Wrapper for a spore type that combines GraphSpore, PulsingSpore and is leveled
  */
trait LevelStruct extends PulsingGraphStruct {
  override type SporeP[P, R] <: LevelSpore[R] with GraphSpore[R] with PulsingSpore[P]
}

/**
  * Wrapper for the instance of LevelSpore
  */
trait SimpleStruct extends LevelStruct {
  override type SporeP[P, R] = LevelSporeImpl[P, R]
}

/**
  * Spore that has a buffered pulse indicating a potential update and storing the updated and the old value.
  * Through the buffer, it is possible to either revert or apply the update
  *
  * @tparam P Pulse stored value type
  */
trait PulsingSpore[P] {
  def pulses: Buffer[Pulse[P]]
}

/**
  * Spore that implements both the buffered pulse and the buffering capabilities itself.
  *
  * @tparam P Pulse stored value type
  */
trait BufferedSpore[P] extends PulsingSpore[P] with Buffer[Pulse[P]] with Committable {
  protected var current: Pulse[P]
  protected val transient: Boolean
  val pulses: Buffer[Pulse[P]] = this
  protected var owner: Turn[_] = null
  private var update: Pulse[P] = Pulse.none

  override def transform(f: (Pulse[P]) => Pulse[P])(implicit turn: Turn[_]): Pulse[P] = {
    val value = f(get)
    set(value)
    value
  }
  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    update = value
    if (owner == null) turn.schedule(this)
    owner = turn
  }
  override def get(implicit turn: Turn[_]): Pulse[P] = { if (turn eq owner) update else current }
  override def base(implicit turn: Turn[_]): Pulse[P] = current

  override def commit(implicit turn: Turn[_]): Unit = {
    if (!transient) current = update.keep
    release(turn)
  }
  override def release(implicit turn: Turn[_]): Unit = {
    update = Pulse.none
    owner = null
  }
}

/**
  * Spore that can represent a node in a graph by providing information about incoming and outgoing edges.
  *
  * @tparam R Type of the reactive values that are connected to this spore
  */
trait GraphSpore[R] {
  def incoming(implicit turn: Turn[_]): Set[R]
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit
  def outgoing(implicit turn: Turn[_]): Set[R]
  def discover(reactive: R)(implicit turn: Turn[_]): Unit
  def drop(reactive: R)(implicit turn: Turn[_]): Unit
}

/**
  * Graph spore that additionally can be assigned a level value that is used for topologically traversing the graph.
  *
  * @tparam R Type of the reactive values that are connected to this spore
  */
trait LevelSpore[R] extends GraphSpore[R] {
  def level(implicit turn: Turn[_]): Int
  def updateLevel(i: Int)(implicit turn: Turn[_]): Int
}

/**
  * Implementation of a spore with graph functionality and a buffered pulse storage.
  *
  * @param current Pulse used as initial value for the spore
  * @param transient If a spore is marked as transient, changes to it can not be committed (and are released instead)
  * @param initialIncoming Initial incoming edges in the spore's graph
  * @tparam P Pulse stored value type
  * @tparam R Type of the reactive values that are connected to this spore
  */
abstract class PropagationSporeImpl[P, R](override var current: Pulse[P], override val transient: Boolean, initialIncoming: Set[R]) extends GraphSpore[R] with BufferedSpore[P] {
  var _incoming: Set[R] = initialIncoming
  var _outgoing: Set[R] = Set.empty

  def incoming(implicit turn: Turn[_]): Set[R] = _incoming
  def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming = reactives
  override def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing
  override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing += reactive
  override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing -= reactive
}

/**
  * Implementation of a spore with graph and buffered pulse storage functionality that also support setting a level.
  *
  * @param current Pulse used as initial value for the spore
  * @param transient If a spore is marked as transient, changes to it can not be committed (and are released instead)
  * @param initialIncoming Initial incoming edges in the spore's graph
  * @tparam P Pulse stored value type
  * @tparam R Type of the reactive values that are connected to this spore
  */
class LevelSporeImpl[P, R](current: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends PropagationSporeImpl[P, R](current, transient, initialIncoming) with LevelSpore[R]  {
  var _level: Int = 0

  override def level(implicit turn: Turn[_]): Int = _level

  override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = {
    val max = math.max(i, _level)
    _level = max
    max
  }
}
