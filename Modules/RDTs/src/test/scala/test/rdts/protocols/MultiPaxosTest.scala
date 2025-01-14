package test.rdts.protocols

import rdts.base.LocalUid
import rdts.datatypes.experiments.protocols.Paxos.given
import rdts.datatypes.experiments.protocols.{MultiPaxos, MultipaxosPhase, Participants, Paxos}

class MultiPaxosTest extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()

  given Participants = Participants(Set(id1, id2, id3).map(_.uid))

  val emptyPaxosObject: MultiPaxos[Int] = MultiPaxos()
  test("happy path") {
    var testPaxosObject = emptyPaxosObject

    assertEquals(testPaxosObject.leader, None)
    assertEquals(testPaxosObject.phase, MultipaxosPhase.LeaderElection, "multipaxos starts in leader election phase")

    val proposeValue = 1
    // replica 1 tries to become leader
    testPaxosObject = testPaxosObject.merge(testPaxosObject.startLeaderElection(using id1))

    // testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id2))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id3))

    assertEquals(testPaxosObject.leader, Some(id1.uid), "id1 should be the leader")

    // replica 2 tries to write
    val afterwrite = testPaxosObject.merge(testPaxosObject.proposeIfLeader(2)(using id2))
    assertEquals(afterwrite, testPaxosObject)

    // replica 1 (leader) writes
    val afterLeaderWrite = testPaxosObject.merge(testPaxosObject.proposeIfLeader(1)(using id1))
    assertNotEquals(afterLeaderWrite, testPaxosObject)
    testPaxosObject = afterLeaderWrite

    assertEquals(testPaxosObject.log, Map.empty)
    assertEquals(testPaxosObject.phase, MultipaxosPhase.Voting)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id3))
    assertEquals(testPaxosObject.log.values.toList, List(1))
    assertEquals(testPaxosObject.phase, MultipaxosPhase.Idle)

    testPaxosObject.merge(testPaxosObject.proposeIfLeader(2)(using id2))

    // replica 1 (leader) writes again
    testPaxosObject = testPaxosObject.merge(testPaxosObject.proposeIfLeader(12)(using id1))
    assertEquals(testPaxosObject.log.values.toList, List(1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id2))
    assertEquals(testPaxosObject.log.values.toList.sorted, List(1, 12))

    // replica 3 starts new leader election
    testPaxosObject = testPaxosObject.merge(testPaxosObject.startLeaderElection(using id3))
    assertEquals(testPaxosObject.log.values.toList.sorted, List(1, 12), "log survives new leader election")
    assertEquals(testPaxosObject.phase, MultipaxosPhase.LeaderElection)
    assertEquals(testPaxosObject.leader, None)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id3))
    assertEquals(testPaxosObject.phase, MultipaxosPhase.LeaderElection)
    assertEquals(testPaxosObject.leader, None)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id2))
    assertEquals(testPaxosObject.phase, MultipaxosPhase.Idle)
    assertEquals(testPaxosObject.leader, Some(id3.uid))
  }

  test("conflicting proposals") {
    var testPaxosObject = emptyPaxosObject

    // replicas 1 and 2 try to become leader
    var rep1 = emptyPaxosObject.merge(emptyPaxosObject.startLeaderElection(using id1))
    var rep2 = emptyPaxosObject.merge(emptyPaxosObject.startLeaderElection(using id2))

    // sync
    testPaxosObject = testPaxosObject.merge(rep1).merge(rep2)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id2))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id3))

    // propose values
    rep1 = testPaxosObject.proposeIfLeader(1)(using id1)
    rep2 = testPaxosObject.proposeIfLeader(2)(using id2)

    // sync
    testPaxosObject = testPaxosObject.merge(rep1).merge(rep2)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id2))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep(using id3))

    assert(testPaxosObject.log.values.head == 1 || testPaxosObject.log.values.head == 2)
  }
}
