import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.experiments.protocols.old.simplified
import rdts.datatypes.experiments.protocols.old.simplified.GeneralizedPaxos
import rdts.datatypes.experiments.protocols.{Consensus, Membership, Paxos}

import scala.util.Try
class MembershipSuite extends munit.ScalaCheckSuite {
//  override def scalaCheckInitialSeed = "6Y9lv63LraBdJTHwHFLm3ItFEF7sm6Ok2D3S22VQcTO="

  // TODO: failing seed
  // override def scalaCheckInitialSeed = "sw9ZNlZ9x0YrVuK2jCaUjc2Im3gi9os7yJbRCA1n8eP="
  // override def scalaCheckInitialSeed = "tuRJ9FeHxNhcTdcocDyhBEuXIokYg0Jyp6Rk9E-KGGF="
  // override def scalaCheckInitialSeed = "JxUkfCJH212GD2NHRYliPeBttGpBvXSqoxX8CP8yhiG="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)
      .withMinSize(100)
      .withMaxSize(500)

  property("Membership with simplepaxos")(MembershipSpec[Int, simplified.Paxos, simplified.Paxos](
    logging = false,
    minDevices = 3,
    maxDevices = 6,
    mergeFreq = 80,
    upkeepFreq = 70,
    writeFreq = 20,
    addMemberFreq = 1,
    removeMemberFreq = 1
  ).property())

  property("Membership with generalized paxos")(MembershipSpec[Int, GeneralizedPaxos, GeneralizedPaxos](
    logging = false,
    minDevices = 3,
    maxDevices = 6,
    mergeFreq = 80,
    upkeepFreq = 70,
    writeFreq = 20,
    addMemberFreq = 1,
    removeMemberFreq = 1
  ).property())

  property("Membership with paper paxos")(MembershipSpec[Int, Paxos, Paxos](
    logging = false,
    minDevices = 3,
    maxDevices = 6,
    mergeFreq = 80,
    upkeepFreq = 70,
    writeFreq = 20,
    addMemberFreq = 1,
    removeMemberFreq = 1
  ).property())

  property("Membership with two different algos")(MembershipSpec[Int, simplified.Paxos, GeneralizedPaxos](
    logging = false,
    minDevices = 3,
    maxDevices = 6,
    mergeFreq = 80,
    upkeepFreq = 70,
    writeFreq = 20,
    addMemberFreq = 1,
    removeMemberFreq = 1
  ).property())
}

class MembershipSpec[A: Arbitrary, C[_]: Consensus, D[_]: Consensus](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    upkeepFreq: Int,
    mergeFreq: Int,
    writeFreq: Int,
    addMemberFreq: Int,
    removeMemberFreq: Int
) extends CommandsARDTs[Membership[A, C, D]] {
  // given logger: Logger = Logger(level = Level.Info)

  given Lattice[Membership[A, C, D]] = Membership.latticeFromConsensus

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, Membership.init[A, C, D](ids.map(_.uid).toSet))).toMap

  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (upkeepFreq, genUpkeep(state)),
      (mergeFreq, genMerge(state)),
      (writeFreq, genWrite(state)),
      (addMemberFreq, genAddMember(state)),
      (removeMemberFreq, genRemoveMember(state))
    )

  def genWrite(state: State): Gen[Write] =
    for
      id    <- genId(state)
      value <- arbitrary[A]
    yield Write(id, value)

  def genMerge(state: State): Gen[Merge] =
    for
      (left, right) <- genId2(state)
    yield Merge(left, right)

  def genUpkeep(state: State): Gen[Upkeep] = genId(state).map(Upkeep(_))

  def genAddMember(state: State): Gen[AddMember] =
    for
      (id1, id2) <- genId2(state)
    yield AddMember(id1, id2)

  def genRemoveMember(state: State): Gen[RemoveMember] =
    for
      (id1, id2) <- genId2(state)
    yield RemoveMember(id1, id2)

  // commands: (merge), upkeep, write, addMember, removeMember
  case class Merge(left: LocalUid, right: LocalUid) extends ACommand(left):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      states(left).merge(states(right))

  case class Write(writer: LocalUid, value: A) extends ACommand(writer):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      val delta = states(writer).write(value)(using writer)
      Lattice.merge(states(writer), delta)

  case class AddMember(adder: LocalUid, newId: LocalUid) extends ACommand(adder):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      val delta = states(adder).addMember(newId.uid)(using adder)
      Lattice.merge(states(adder), delta)

  case class RemoveMember(remover: LocalUid, removeId: LocalUid) extends ACommand(remover):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      val delta = states(remover).removeMember(removeId.uid)(using remover)
      Lattice.merge(states(remover), delta)

  case class Upkeep(id: LocalUid) extends ACommand(id):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      val delta = states(id).upkeep()(using id)
      Lattice.merge(states(id), delta)

    override def postCondition(state: State, result: Try[Result]): Prop =
      val res: Map[LocalUid, Membership[A, C, D]] = result.get
      Prop.forAll(genId2(res)) {
        (index1, index2) =>
          (res(index1), res(index2)) match
            case (membership1, membership2) =>
              (membership1.currentMembers.nonEmpty && membership2.currentMembers.nonEmpty) :| "set of members can never be empty" &&
              ((membership1.rounds.counter != membership2.rounds.counter) || membership1.currentMembers == membership2.currentMembers) :| "members for a given counter are the same for all indices" &&
              (membership1.read.containsSlice(membership2.read) || membership2.read.containsSlice(
                membership1.read
              )) :| s"every log is a prefix of another log or vice versa, but we had:\n${membership1.rounds.counter}${membership1.read}\n${membership2.rounds.counter}${membership2.read}"

      }

}
