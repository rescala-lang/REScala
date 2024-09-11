import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{Consensus, LogHack, Membership, Paxos, simplified}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
class MembershipSuite extends munit.ScalaCheckSuite {
  override def scalaCheckInitialSeed = "6Y9lv63LraBdJTHwHFLm3ItFEF7sm6Ok2D3S22VQcTO="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)
      .withMinSize(100)
      .withMaxSize(500)

  property("Membership with simplepaxos")(MembershipSpec[Int, simplified.Paxos, simplified.Paxos](
    minDevices = 3,
    maxDevices = 6
  ).property())
}

class MembershipSpec[A: Arbitrary, C[_]: Consensus, D[_]: Consensus](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int
)(using
    Bottom[C[Set[Uid]]],
    Bottom[D[A]],
    Lattice[C[Set[Uid]]],
    Lattice[D[A]],
    Lattice[Membership[A,C,D]]
) extends CommandsARDTs[Membership[A, C, D]] {
  // given logger: Logger = Logger(level = Level.Info)

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, Membership.init[A, C, D](ids.map(_.uid).toSet))).toMap

  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (70, genUpkeep(state)),
      (80, genMerge(state)),
      (5, genWrite(state)),
//      (10, genRead(state))
//      (2, genAddMember(state)),
//      (2, genRemoveMember(state))
    )

  def genWrite(state: State): Gen[Write] =
    for
      id <- genId(state)
      value <- arbitrary[A]
    yield Write(id, value)

  def genRead(state: State): Gen[Read] = genId(state).map(Read(_))

  def genUpkeep(state: State): Gen[Upkeep] = genId(state).map(Upkeep(_))

//  def genAddMember(state: State): Gen[AddMember] =
//    for
//      (id1, id2)    <- genId2(state)
//    yield AddMember(id1, id2)
//
//  def genRemoveMember(state: State): Gen[RemoveMember] =
//    for
//      (id1, id2)    <- genId2(state)
//    yield RemoveMember(id1, id2)

  // commands: merge, upkeep, read, write, addMember, removeMember
//  case class Merge(leftIndex: Int, rightIndex: Int) extends Command:
//    type Result = Membership[Int, C, D]
//
//    def newLocalState(states: Seq[(LocalUid, LocalState)]): (LocalUid, LocalState) =
//      val (idL, membershipL) = states(leftIndex)
//      val (idR, membershipR) = states(rightIndex)
//      //      println(s"merging ${states(leftIndex)} with ${states(rightIndex)}")
//      (idL, membershipL.merge(membershipR))
//
//    override def run(sut: Sut): Result =
//      sut(leftIndex) = newLocalState(sut.toSeq)
//      newLocalState(sut.toSeq)._2
//
//    override def nextState(state: State): State =
//      state.updated(leftIndex, newLocalState(state))
//
//    override def preCondition(state: State): Boolean =
//      leftIndex >= 0 && leftIndex < state.length &&
//      rightIndex >= 0 && rightIndex < state.length
//      && leftIndex != rightIndex
//
//    override def postCondition(state: State, result: Try[Result]): Prop =
//      val (leftId, leftMembership)   = state(leftIndex)
//      val (rightId, rightMembership) = state(rightIndex)
//
////      def allUids(p: Paxos[?]): Set[Uid] =
////        p.prepares.map(_.proposer) union
////        p.promises.flatMap(p => Set(p.proposal.proposer, p.acceptor)) union
////        p.accepts.map(_.proposal.proposer) union
////        p.accepteds.flatMap(p => Set(p.proposal.proposer, p.acceptor))
////
////      (leftMembership.counter != rightMembership.counter || (allUids(leftMembership.innerConsensus) union allUids(
////        rightMembership.innerConsensus
////      ) union allUids(leftMembership.membersConsensus) union allUids(rightMembership.membersConsensus)).subsetOf(
////        leftMembership.currentMembers
////      )) :| s"updates only contain Uids of known members. Tried merging ${allUids(leftMembership.innerConsensus)} and ${allUids(rightMembership.innerConsensus)}" &&
//      ((leftMembership.counter != rightMembership.counter) || leftMembership.currentMembers == rightMembership.currentMembers) :| "when two instances have the same counter, they have to have the same set of members" &&
//      result.isSuccess
//  //      Lattice[Membership[Int, Paxos, Paxos]].lteq(state(rightIndex)._2, result.get) :| "Merge produces valid results"

  case class Write(writer: LocalUid, value: A) extends ACommand(writer):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      val delta = states(writer).write(value)(using writer)
      Lattice.merge(states(writer), delta)

  case class Read(reader: LocalUid) extends ACommand(reader):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      states(reader)

    override def postCondition(state: State, result: Try[Result]): Prop =
      val readLog: Seq[A] = result.get(reader).read
      // every log is a prefix of another log or vice versa
      Prop.forAll(genId(state)) {
        id =>
              (state(id).read.containsSlice(readLog) || readLog.containsSlice(
                state(id).read
              )) :| "every log is a prefix of another log or vice versa"
      }

//  case class AddMember(adder: LocalUid, newId: LocalUid) extends UnitCommand:
//    override def run(sut: Sut): Unit =
//      val (id, membership) = sut(id)
//      sut(id) = (id, membership.merge(membership.addMember(newId.uid)(using id)))
//
//    override def nextState(state: List[(LocalUid, LocalState)]): List[(LocalUid, LocalState)] =
//      val (id, membership) = state(index)
//      assert(membership.currentMembers.nonEmpty, s"members should not be empty: $state")
//      state
//        .updated(index, (id, membership.merge(membership.addMember(newId.uid)(using id))))
//
//    override def preCondition(state: List[(LocalUid, LocalState)]): Boolean = true
//
//    override def postCondition(state: State, success: Boolean): Prop = success
//
//  case class RemoveMember(remover: LocalUid, removeId: LocalUid) extends UnitCommand:
//    override def run(sut: Sut): Unit =
//      val (id, membership) = sut(index)
//      sut(index) = (id, membership.merge(membership.removeMember(removeId.uid)(using id)))
//
//    override def nextState(state: List[(LocalUid, LocalState)]): List[(LocalUid, LocalState)] =
//      val (id, membership) = state(index)
//      state.updated(index, (id, membership.merge(membership.removeMember(removeId.uid)(using id))))
//
//    override def preCondition(state: List[(LocalUid, LocalState)]): Boolean = true
//
//    override def postCondition(state: State, success: Boolean): Prop = success

  case class Upkeep(id: LocalUid) extends ACommand(id):
    override def nextLocalState(states: Map[LocalUid, Membership[A, C, D]]): Membership[A, C, D] =
      val delta = states(id).upkeep()(using id, LogHack(true))
      Lattice.merge(states(id), delta)

    override def postCondition(state: State, result: Try[Result]): Prop =
      val res: Map[LocalUid, Membership[A,C,D]] = result.get
      Prop.forAll(genId2(res)) {
        (index1, index2) =>
          (res(index1), res(index2)) match
            case (membership1, membership2) =>
              (membership1.membersConsensus.members == membership1.innerConsensus.members) :| "members of both protocols never go out of sync" &&
              ((membership1.counter != membership2.counter) || membership1.currentMembers == membership2.currentMembers) :| "members for a given counter are the same for all indices" &&
              (membership1.read.containsSlice(membership2.read) || membership2.read.containsSlice(membership1.read)) :| "every log is a prefix of another log or vice versa"

      }

}
