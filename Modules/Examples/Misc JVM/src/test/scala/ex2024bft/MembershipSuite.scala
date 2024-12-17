package ex2024bft

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.old.Paxos

import scala.collection.mutable.ArrayBuffer

class MembershipSuite extends munit.ScalaCheckSuite {

//  override def scalaCheckTestParameters =
//    super.scalaCheckTestParameters
//      .withMinSuccessfulTests(1000)
//      .withMinSize(100)
//      .withMaxSize(300)

  property("Membership specification")(MembershipSpec.property())
}

import scala.util.Try

object MembershipSpec extends Commands {
  //// config
  val minDevices = 3
  val maxDevices = 3
  // given logger: Logger = Logger(level = Level.Info)
  ////

  type State      = List[(LocalUid, LocalState)]
  type LocalState = Membership[Int, Paxos, Paxos]

  override type Sut = scala.collection.mutable.ArrayBuffer[(LocalUid, LocalState)]

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def newSut(state: State): Sut = ArrayBuffer.from(state)

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, maxDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, Membership.init[Int, Paxos, Paxos](ids.map(_.uid).toSet)))

  override def genCommand(state: State): Gen[MembershipSpec.Command] =
    Gen.frequency(
      (70, genUpkeep(state)),
      (80, genMerge(state)),
      (30, genWrite(state)),
      (10, genRead(state)),
      (2, genAddMember(state)),
      (2, genRemoveMember(state))
    )

  def genIndex(state: State): Gen[Int] = Gen.choose(0, state.length - 1)

  def genIndex2(state: State): Gen[(Int, Int)] =
    for
      leftIndex <- genIndex(state)
      rightIndex = (leftIndex + 1) % state.length
    yield (leftIndex, rightIndex)

  def genMerge(state: State): Gen[Merge] =
    genIndex2(state).map((l: Int, r: Int) => Merge(l, r))

  def genWrite(state: State): Gen[Write] =
    for
      index <- genIndex(state)
      value <- arbitrary[Int]
    yield Write(index, value)

  def genRead(state: State): Gen[Read] = genIndex(state).map(Read(_))

  def genUpkeep(state: State): Gen[Upkeep] = genIndex(state).map(Upkeep(_))

  def genAddMember(state: State): Gen[AddMember] =
    for
      index    <- genIndex(state)
      addIndex <- genIndex(state)
      addId = state(addIndex)._1
    yield AddMember(index, addId)

  def genRemoveMember(state: State): Gen[RemoveMember] =
    for
      index       <- genIndex(state)
      removeIndex <- genIndex(state)
      removeId = state(removeIndex)._1
    yield RemoveMember(index, removeId)

  // commands: merge, upkeep, read, write, addMember, removeMember
  case class Merge(leftIndex: Int, rightIndex: Int) extends Command:
    type Result = Membership[Int, Paxos, Paxos]

    def newLocalState(states: Seq[(LocalUid, LocalState)]) =
      val (idL, membershipL) = states(leftIndex)
      val (idR, membershipR) = states(rightIndex)
      //      println(s"merging ${states(leftIndex)} with ${states(rightIndex)}")
      (idL, membershipL.merge(membershipR))

    override def run(sut: Sut): Result =
      sut(leftIndex) = newLocalState(sut.toSeq)
      newLocalState(sut.toSeq)._2

    override def nextState(state: State): State =
      state.updated(leftIndex, newLocalState(state))

    override def preCondition(state: State): Boolean =
      leftIndex >= 0 && leftIndex < state.length &&
      rightIndex >= 0 && rightIndex < state.length
      && leftIndex != rightIndex

    override def postCondition(state: State, result: Try[Result]): Prop =
      val (leftId, leftMembership)   = state(leftIndex)
      val (rightId, rightMembership) = state(rightIndex)

      def allUids(p: Paxos[?]): Set[Uid] =
        p.prepares.map(_.proposer) union
        p.promises.flatMap(p => Set(p.proposal.proposer, p.acceptor)) union
        p.accepts.map(_.proposal.proposer) union
        p.accepteds.flatMap(p => Set(p.proposal.proposer, p.acceptor))

      (leftMembership.counter != rightMembership.counter || (allUids(leftMembership.innerConsensus) union allUids(
        rightMembership.innerConsensus
      ) union allUids(leftMembership.membersConsensus) union allUids(rightMembership.membersConsensus)).subsetOf(
        leftMembership.currentMembers
      )) :| s"updates only contain Uids of known members. Tried merging ${allUids(leftMembership.innerConsensus)} and ${allUids(rightMembership.innerConsensus)}" &&
      ((leftMembership.counter != rightMembership.counter) || leftMembership.currentMembers == rightMembership.currentMembers) :| "when two instances have the same counter, they have to have the same set of members" &&
      result.isSuccess
  //      Lattice[Membership[Int, Paxos, Paxos]].lteq(state(rightIndex)._2, result.get) :| "Merge produces valid results"

  case class Write(index: Int, value: Int) extends UnitCommand:
    def newLocalState(states: Seq[(LocalUid, LocalState)]) =
      val (id, membership) = states(index)
      (id, membership.merge(membership.write(value)(using id)))

    override def run(sut: Sut): Unit =
      sut(index) = newLocalState(sut.toSeq)

    override def nextState(state: State): State =
      state.updated(index, newLocalState(state))

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, success: Boolean): Prop = success

  case class Read(index: Int) extends Command:
    override type Result = List[Int]

    override def run(sut: Sut): Result =
      val (id, membership) = sut(index)
      val res              = membership.read
      //      if res.nonEmpty then
      //        println(s"members: ${membership.currentMembers}, res: $res, innerConsensus: ${membership.innerConsensus}")
      res

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      // every log is a prefix of another log or vice versa
      Prop.forAll(genIndex(state)) {
        index =>
          state(index) match
            case (id, membership) =>
              (membership.read.containsSlice(result.get) || result.get.containsSlice(
                membership.read
              )) :| "every log is a prefix of another log or vice versa"
      }

  case class AddMember(index: Int, newId: LocalUid) extends UnitCommand:
    override def run(sut: Sut): Unit =
      val (id, membership) = sut(index)
      sut(index) = (id, membership.merge(membership.addMember(newId.uid)(using id)))

    override def nextState(state: List[(LocalUid, LocalState)]): List[(LocalUid, LocalState)] =
      val (id, membership) = state(index)
      assert(membership.currentMembers.nonEmpty, s"members should not be empty: $state")
      state
        .updated(index, (id, membership.merge(membership.addMember(newId.uid)(using id))))

    override def preCondition(state: List[(LocalUid, LocalState)]): Boolean = true

    override def postCondition(state: State, success: Boolean): Prop = success

  case class RemoveMember(index: Int, removeId: LocalUid) extends UnitCommand:
    override def run(sut: Sut): Unit =
      val (id, membership) = sut(index)
      sut(index) = (id, membership.merge(membership.removeMember(removeId.uid)(using id)))

    override def nextState(state: List[(LocalUid, LocalState)]): List[(LocalUid, LocalState)] =
      val (id, membership) = state(index)
      state.updated(index, (id, membership.merge(membership.removeMember(removeId.uid)(using id))))

    override def preCondition(state: List[(LocalUid, LocalState)]): Boolean = true

    override def postCondition(state: State, success: Boolean): Prop = success

  case class Upkeep(index: Int) extends Command:
    type Result = Sut

    def newLocalState(states: Seq[(LocalUid, LocalState)]) =
      val (id, membership) = states(index)
      (id, membership.merge(membership.upkeep()(using id)))

    override def run(sut: Sut): Result =
      val newState = newLocalState(sut.toSeq)
      //      if newState._2.counter > sut(index)._2.counter then
      //        println(newState)
      sut(index) = newLocalState(sut.toSeq)
      sut

    override def nextState(state: State): State =
      state.updated(index, newLocalState(state))

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      Prop.forAll(genIndex2(result.get.toList)) {
        (index1, index2) =>
          (result.get(index1), result.get(index2)) match
            case ((_, membership1), (_, membership2)) =>
              (membership1.membersConsensus.members == membership1.innerConsensus.members) :| "members of both protocols never go out of sync" &&
              ((membership1.counter != membership2.counter) || membership1.currentMembers == membership2.currentMembers) :| "members for a given counter are the same for all indices"

      }

}
//class PaxosPropertyTests {
//
//  val numParticipants                  = Random.between(3, 6)
//  val participants: Map[Int, LocalUid] = Range(start = 0, end = numParticipants).map(num => (num, LocalUid.gen())).toMap
//
//  enum Action:
//    case Merge        extends Action
//    case Upkeep       extends Action
//    case Read         extends Action
//    case Write        extends Action
//    case AddMember    extends Action
//    case RemoveMember extends Action
//
//  val actions = Action.values
//
//  val lenTrace = 50
//  val Trace    = Range(0, lenTrace).map(_ => actions(Random.nextInt(actions.size)))
//
//// action probabilities
////
//// -> generate list of actions
//// -> enrich actions with parameters
//// -> compress action list by merging subsets
//
//}
