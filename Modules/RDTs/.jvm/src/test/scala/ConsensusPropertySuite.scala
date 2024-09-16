import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.datatypes.experiments.protocols.{Consensus, simplified}
import org.scalacheck.commands.Commands
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{Consensus, LogHack, Membership, Paxos, simplified}

import scala.collection.mutable
import scala.util.Try

class PaxosSimplifiedSuite extends munit.ScalaCheckSuite {

//  override def scalaCheckInitialSeed = "yL7jhVAhl4I5iCmRP_WmL07-3jaoICgGS7X0-zv54LD="

//  override def scalaCheckTestParameters =
//    super.scalaCheckTestParameters
//      .withMinSuccessfulTests(500)
//      .withMinSize(30)
//      .withMaxSize(200)

  property("Paxos simplified")(SimplePaxosSuite[Int](
    logging = false,
    minDevices = 3,
    maxDevices = 3,
    writeFreq = 20,
    upkeepFreq = 70,
    mergeFreq = 70
  ).property())
}

class SimplePaxosSuite[A: Arbitrary](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    writeFreq: Int,
    upkeepFreq: Int,
    mergeFreq: Int
) extends ConsensusPropertySuite[A, simplified.Paxos](
      logging,
      minDevices,
      maxDevices,
      writeFreq,
      upkeepFreq,
      mergeFreq
    ) {

  override def genUpkeep(state: Map[LocalUid, simplified.Paxos[A]]): Gen[Upkeep] =
    for
      id <- genId(state)
    yield PUpkeep(id)

  class PUpkeep(id: LocalUid) extends Upkeep(id) {
    override def postCondition(
        state: Map[LocalUid, simplified.Paxos[A]],
        result: Try[Map[LocalUid, simplified.Paxos[A]]]
    ): Prop =
      val res            = result.get
      val doubleProposal = res(id).accepts.groupBy(_.proposal).find(_._2.size > 1)
      doubleProposal.isEmpty :| s"Only one proposal for a given proposal number. Found: $doubleProposal" &&
      super.postCondition(state, result)
  }
}

class ConsensusPropertySuite[A: Arbitrary, C[_]: Consensus](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    writeFreq: Int,
    upkeepFreq: Int,
    mergeFreq: Int
)(using Lattice[C[A]]) extends CommandsARDTs[C[A]] {

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, Consensus.init(ids.map(_.uid).toSet))).toMap

  // generators
  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (writeFreq, genWrite(state)),
      (upkeepFreq, genUpkeep(state)),
      (mergeFreq, genMerge(state))
    )

  def genWrite(state: State): Gen[Write] =
    for
      id    <- genId(state)
      value <- arbitrary[A]
    yield Write(id, value)

  def genUpkeep(state: State) =
    genId(state).map(Upkeep(_))

  // commands that change state
  class Write(writer: LocalUid, value: A) extends ACommand(writer):
    def nextLocalState(states: Map[LocalUid, C[A]]) =
      Lattice[C[A]].merge(states(writer), states(writer).write(value)(using writer))

    override def postCondition(state: Map[LocalUid, C[A]], result: Try[Map[LocalUid, C[A]]]) =
      (state(writer).members == result.get(writer).members) :| s"Members do not change during writes.\nBefore: ${state(writer)}\nAfter:${result.get(writer)}"

  class Upkeep(id: LocalUid) extends ACommand(id):
    def nextLocalState(states: Map[LocalUid, C[A]]) =
      Lattice[C[A]].merge(states(id), states(id).upkeep()(using id))

    override def postCondition(state: Map[LocalUid, C[A]], result: Try[Map[LocalUid, C[A]]]): Prop =
      val res      = result.get
      val resValue = res(id).read

      if logging && resValue.nonEmpty then println(s"accepted value: ${resValue.get}")

      // if two devices read a value it has to be the same
      Prop.forAll(genId(res)) {
        index =>
          (resValue, res(index).read) match
            case (Some(v1), Some(v2)) =>
              (v1 == v2) :| s"if two devices read a value, it has to be the same, got $v1, $v2" &&
              res(index).members.nonEmpty :| s"members are never empty" &&
              (state(index).members == res(index).members) :| s"upkeep does not change members"
            case _ => Prop(true)
      }
}

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
