package test.rdts.protocols
import munit.ScalaCheckSuite
import org.scalacheck.commands.Commands
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Properties
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{Membership, Paxos}

class MembershipSuite extends ScalaCheckSuite {
  property("Membership works")(MembershipSpec.property())
}

import scala.util.{Success, Try}
import scala.util.Random

object MembershipSpec extends Commands {
  type State = List[(LocalUid, Membership[Int, Paxos, Paxos])]

  override type Sut = State

  override def canCreateNewSut(
      newState: State,
      initSuts: Iterable[State],
      runningSuts: Iterable[Sut]
  ): Boolean = true

  override def newSut(state: State): Sut = state

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] =
    val genMembership: Gen[Membership[Int, Paxos, Paxos]] = Gen.oneOf(List(Membership.unchanged[Int, Paxos, Paxos]))
    val genDevice = for {
      membership <- genMembership
      id = LocalUid.gen()
    } yield (id, membership)
    val numDevices = Gen.choose(3, 6)
    numDevices.flatMap(n => Gen.listOfN(n, genDevice))

  override def genCommand(state: State): Gen[MembershipSpec.Command] = Gen.oneOf(genMerge, genMerge)

  // commands: merge, upkeep, read, write, addMember, removeMember
  case class Merge(leftIndex: Int, rightIndex: Int) extends UnitCommand:
    override def run(sut: Sut): Unit = ()
    override def nextState(state: State): State =
      val (leftId, leftState)   = state(leftIndex)
      val (rightId, rightState) = state(rightIndex)
      val newState              = Lattice[Membership[Int, Paxos, Paxos]].merge(leftState, rightState)
      state.updated(leftIndex, (leftId, newState))

    override def preCondition(state: State): Boolean =
      leftIndex >= 0 && leftIndex < state.length &&
      rightIndex >= 0 && rightIndex < state.length &&
      leftIndex != rightIndex

    override def postCondition(state: State, success: Boolean): Prop = success

  val genMerge: Gen[Merge] = Gen.resultOf(Merge(_, _))
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
