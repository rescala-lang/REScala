package test.rdts
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.experiments.protocols.MultipaxosPhase.LeaderElection
import rdts.datatypes.experiments.protocols.{MultiPaxos, Participants}

import scala.util.Try

extension [A](s1: Seq[A])
  def isPrefix(s2: Seq[A]): Boolean = s1.indexOfSlice(s2) == 0

class MultiPaxosSuite extends munit.ScalaCheckSuite {
  // override def scalaCheckInitialSeed = "UyoN51e59jSOQrryl2a6RnndAVJ0m290IRZ8JjXL9rJ="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)
      .withMinSize(30)
      .withMaxSize(500)

  property("Multipaxos")(MultiPaxosSpec[Int](
    logging = false,
    minDevices = 3,
    maxDevices = 5,
    proposeFreq = 5,
    startElectionFreq = 1,
    mergeFreq = 80
  ).property())
}

class MultiPaxosSpec[A: Arbitrary](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    proposeFreq: Int,
    startElectionFreq: Int,
    mergeFreq: Int
) extends CommandsARDTs[MultiPaxos[A]] {

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, MultiPaxos())).toMap

  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (mergeFreq, genMerge(state)),
      (proposeFreq, genPropose(state)),
      (startElectionFreq, genStartElection(state))
    )

  def genPropose(state: State): Gen[Propose] =
    for
      id    <- genId(state)
      value <- arbitrary[A]
    yield Propose(id, value)

  def genStartElection(state: State): Gen[StartElection] = genId(state).map(StartElection(_))

  def genMerge(state: State): Gen[Merge] =
    for
      (left, right) <- genId2(state)
    yield Merge(left, right)

  // commands: (merge), upkeep, write, addMember, removeMember
  case class Merge(left: LocalUid, right: LocalUid) extends ACommand(left):
    override def nextLocalState(states: Map[LocalUid, MultiPaxos[A]]): MultiPaxos[A] =
      given Participants(states.keySet.map(_.uid))
      val merged = states(left).merge(states(right))
      val result = merged.merge(merged.upkeep(using left))

      if logging then
        if result.read.length > 1 then
          println(result)
      result

    override def postCondition(state: State, result: Try[Result]): Prop =
      given Participants(state.keySet.map(_.uid))
      val res: Map[LocalUid, MultiPaxos[A]] = result.get
      Prop.forAll(genId2(res)) {
        (index1, index2) =>
          (state(index1), state(index2), res(index1), res(index2)) match
            case (oldMultipaxos1, oldMultipaxos2, multipaxos1, multipaxos2) =>
              val (log1, log2)       = (multipaxos1.read, multipaxos2.read)
              val (oldLog1, oldLog2) = (oldMultipaxos1.read, oldMultipaxos2.read)
              (log1.isPrefix(log2) || log2.isPrefix(
                log1
              )) :| s"every log is a prefix of another log or vice versa, but we had:\n${multipaxos1.rounds.counter}${multipaxos1.read}\n${multipaxos2.rounds.counter}${multipaxos2.read}" &&
              // ((multipaxos1.rounds.counter != multipaxos2.rounds.counter) || multipaxos1.leader.isEmpty || multipaxos2.leader.isEmpty || (multipaxos1.leader == multipaxos2.leader)) :| s"There can only ever be one leader for a given epoch but we got:\n${multipaxos1.leader}\n${multipaxos2.leader}" &&
              (log1.isPrefix(oldLog1) && log2.isPrefix(oldLog2)) :| s"logs never shrink" &&
              (multipaxos1.phase != LeaderElection || multipaxos1.leader.isEmpty) && (multipaxos1.phase == LeaderElection || multipaxos1.leader.nonEmpty) :| s"leader is only undefined during leader election, got ${multipaxos1.leader} in phase ${multipaxos1.phase}"
      }

  case class Propose(proposer: LocalUid, value: A) extends ACommand(proposer):
    override def nextLocalState(states: Map[LocalUid, MultiPaxos[A]]): MultiPaxos[A] =
      given Participants(states.keySet.map(_.uid))
      val delta    = states(proposer).proposeIfLeader(value)(using proposer)
      val proposed = Lattice.merge(states(proposer), delta)
      Lattice.merge(proposed, proposed.upkeep(using proposer))

  case class StartElection(initiator: LocalUid) extends ACommand(initiator):
    override def nextLocalState(states: Map[LocalUid, MultiPaxos[A]]): MultiPaxos[A] =
      given Participants(states.keySet.map(_.uid))
      val delta  = states(initiator).startLeaderElection(using initiator)
      val merged = Lattice.merge(states(initiator), delta)
      Lattice.merge(merged, merged.upkeep(using initiator))
}
