package test.rdts.protocols

import munit.ScalaCheckSuite
import org.scalacheck.commands.Commands
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{Consensus, Membership, Paxos}
import rdts.time.Time

class MembershipSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)
      .withMinSize(100)
      .withMaxSize(300)

  property("Membership specification")(MembershipSpec.property())
}

import scala.util.{Success, Try}
import scala.util.Random

object MembershipSpec extends Commands {
  //// config
  val minDevices = 3
  val maxDevices = 3
  ////

  //  case class LocalState(counter: Time, members: Set[Uid], log: List[Int])

  type State = List[(LocalUid, LocalState)]
  type LocalState = Membership[Int, Paxos, Paxos]

  override type Sut = Array[(LocalUid, Membership[Int, Paxos, Paxos])]

  override def canCreateNewSut(
                                newState: State,
                                initSuts: Iterable[State],
                                runningSuts: Iterable[Sut]
                              ): Boolean = true

  override def newSut(state: State): Sut = state.toArray

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, maxDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, Membership.init[Int, Paxos, Paxos](ids.map(_.uid).toSet)))

  override def genCommand(state: State): Gen[MembershipSpec.Command] = Gen.frequency(
    (70, genUpkeep(state)),
    (80, genMerge(state)),
    (30, genWrite(state)),
    (10, genRead(state))
  )

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
      Lattice[Membership[Int, Paxos, Paxos]].lteq(state(rightIndex)._2, result.get)

  def genIndex(state: State): Gen[Int] = Gen.choose(0, state.length - 1)

  def genMerge(state: State): Gen[Merge] =
    for
      leftIndex <- genIndex(state)
      rightIndex = (leftIndex + 1) % state.length
    yield Merge(leftIndex, rightIndex)

  def genWrite(state: State): Gen[Write] =
    for
      index <- genIndex(state)
      value <- arbitrary[Int]
    yield Write(index, value)

  def genRead(state: State): Gen[Read] = genIndex(state).map(Read(_))

  def genUpkeep(state: State): Gen[Upkeep] = genIndex(state).map(Upkeep(_))

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
      val res = membership.read
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
              (membership.read.containsSlice(result.get) || result.get.containsSlice(membership.read)) :| "every log is a prefix of another log or vice versa"
      }

  case class Upkeep(index: Int) extends UnitCommand:
    def newLocalState(states: Seq[(LocalUid, LocalState)]) =
      val (id, membership) = states(index)
      (id, membership.merge(membership.upkeep()(using id)))

    override def run(sut: Sut): Unit =
      val newState = newLocalState(sut.toSeq)
      if newState._2.counter > sut(index)._2.counter then
        println(newState)
      sut(index) = newLocalState(sut.toSeq)

    override def nextState(state: State): State =
      state.updated(index, newLocalState(state))

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, success: Boolean): Prop =
      Prop.forAll(genIndex(state)) {
        index =>
          state(index) match
            case (id, membership) =>
              (membership.membersConsensus.members == membership.innerConsensus.members) :| "members of both protocols never go out of sync"
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
