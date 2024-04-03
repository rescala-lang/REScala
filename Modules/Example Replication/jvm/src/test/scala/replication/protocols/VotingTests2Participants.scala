package replication.protocols
import rdts.base.Uid
import rdts.datatypes.contextual.ReplicatedSet
import rdts.datatypes.{Epoch, LastWriterWins}
import rdts.syntax.LocalReplicaId
import rdts.time.Dots

class VotingTests2Participants extends munit.FunSuite {
  given dots: Dots = Dots.empty
  // create replicas for set of 2 participants
  val id1: LocalReplicaId = LocalReplicaId.gen()
  val id2: LocalReplicaId = LocalReplicaId.gen()
  var voting = Voting(
    rounds = Epoch.empty[ReplicatedSet[Vote]],
    numParticipants = LastWriterWins.now(2)
  )
  test("No initial owner") {
    assert(!voting.isOwner(using id1))
  }
  test("Still not owner after one vote") {
    voting = voting.merge(voting.voteFor(id1.uid)(using id1).data)
    assert(!voting.isOwner(using id1))
  }
  test("Duplicate vote changes nothing") {
    assertEquals(voting.merge(voting.voteFor(id1.uid)(using id1).data), voting)
  }
  test("Is owner after two votes") {
    voting = voting.merge(voting.voteFor(id1.uid)(using id2).data)
    assert(voting.isOwner(using id1))
    assert(!voting.isOwner(using id2))
  }
  test("Is not owner for 4 participants") {
    voting = voting.merge(Voting(voting.rounds, LastWriterWins.now(4)))
    assert(!voting.isOwner(using id1))
    assert(!voting.isOwner(using id2))
  }
  test("Is owner for 3 participants") {
    voting = voting.merge(Voting(voting.rounds, LastWriterWins.now(3)))
    assert(voting.isOwner(using id1))
    assert(!voting.isOwner(using id2))
  }
}
