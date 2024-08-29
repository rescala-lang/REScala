import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import rdts.base.{Lattice, LocalUid}

import scala.collection.mutable
import scala.util.Try

/** An API for stateful testing of ARDTs, removing a lot of the clutter of ScalaCheck's Commands API.
  * Users should use the trait `ACommand` for their commands which only needs an implementation of `nextLocalState` which is a function from a map of states to the next local state.
  * @tparam LocalState the type of the ARDT
  */
trait CommandsARDTs[LocalState: Lattice] extends Commands:
  override type State = Map[LocalUid, LocalState]
  override type Sut   = scala.collection.mutable.Map[LocalUid, LocalState]

  override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]): Boolean = true

  override def newSut(state: State): Sut = mutable.Map.from(state)

  override def destroySut(sut: Sut): Unit = sut.clear()

  override def initialPreCondition(state: State): Boolean = true

  def genId(state: State): Gen[LocalUid] = Gen.oneOf(state.keys)

  def genId2(state: State): Gen[(LocalUid, LocalUid)] =
    val ids = state.keys.toList
    for
      leftIndex <- Gen.choose(0, ids.length - 1)
      offset    <- Gen.choose(1, ids.length - 1)
      rightIndex = (leftIndex + offset) % ids.length
    yield (ids(leftIndex), ids(rightIndex))

  def genMerge(state: State): Gen[Merge] =
    for
      (left, right) <- genId2(state)
    yield Merge(left, right)

  trait ACommand(id: LocalUid) extends Command:
    override type Result = State
    def nextLocalState(states: State): LocalState

    override def run(sut: Sut): Result =
      sut.update(id, nextLocalState(sut.toMap))
      sut.toMap

    override def nextState(state: State) =
      state.updated(id, nextLocalState(state))

    override def preCondition(state: Map[LocalUid, LocalState]) = true

    override def postCondition(state: Map[LocalUid, LocalState], result: Try[Result]): Prop = result.isSuccess

  class Merge(left: LocalUid, right: LocalUid) extends ACommand(left):
    def nextLocalState(states: Map[LocalUid, LocalState]) =
      Lattice[LocalState].merge(states(left), states(right))
