package replication.protocols

import rdts.time.Dots
import rdts.base.{Lattice, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
import rdts.syntax.LocalReplicaId

class TokensTest extends munit.FunSuite {
  given dots: Dots                  = Dots.empty
  given Lattice[Dotted[Token]]      = Lattice.derived
  val numOfReplicas                 = 5
  val replicas: Seq[LocalReplicaId] = List.tabulate(numOfReplicas)(_ => LocalReplicaId.gen())
  var token = Dotted(Token(
    os = Ownership.unchanged,
    wants = ReplicatedSet.empty
  ))
  // set replica 0 the initial owner
  token = token.merge(Dotted(Token(Ownership(1, LocalReplicaId.unwrap(replicas(0))), ReplicatedSet.empty)))

  test("Some replica initially owns the token") {
    assert(List.range(0, numOfReplicas).map(n => token.data.isOwner(using replicas(n))).reduce((x, y) => x || y))
  }
  test("Owner doesn't change if no replica wants the token") {
    // replica 0, the current owner, calls upkeep
    val updatedToken = token.merge(Dotted(token.data.upkeep(using replicas(0))))
    // wants is empty, owner should remain same
    assert(updatedToken.data.isOwner(using replicas(0)))
  }
  test("Interested replica with the biggest id gets the token next") {
    // replicas 2 and 3 request token
    var updatedToken = token.merge(token.data.request(using replicas(1)))
    updatedToken = updatedToken.merge(updatedToken.data.request(using replicas(2)))
    // find biggest id in wants
    val biggestIdIndex = replicas.indexOf(LocalReplicaId.predefined(updatedToken.data.wants.elements.max))
    // replica 0, the current owner, calls upkeep
    updatedToken = updatedToken.merge(Dotted(updatedToken.data.upkeep(using replicas(0))))
    // assert that the new owner is the one with the biggest id
    assert(updatedToken.data.isOwner(using replicas(biggestIdIndex)))
  }
  test("Replica that isn't the owner can't change owner") {
    // replica 1 is not the owner and calls upkeep
    val updatedToken = token.merge(Dotted(token.data.upkeep(using replicas(1))))
    assert(updatedToken.data.isOwner(using replicas(0)))
  }
  test("Owner can't choose itself as the next owner if other replicas interested") {
    // replica 1 interested
    var updatedToken = token.merge(token.data.request(using replicas(1)))
    // replica 0 calls upkeep, 1 should be the new owner
    updatedToken = updatedToken.merge(Dotted(updatedToken.data.upkeep(using replicas(0))))
    // assert that replica 0 is not the new owner
    assert(!updatedToken.data.isOwner(using replicas(0)))
  }
  test("Replica that never requested can't be chosen to be the owner") {
    // replicas 1 & 2 request the token
    var updatedToken = token.merge(token.data.request(using replicas(1)))
    updatedToken = updatedToken.merge(token.data.request(using replicas(2)))
    // replica 0 calls upkeep
    updatedToken = updatedToken.merge(Dotted(updatedToken.data.upkeep(using replicas(0))))
    // assert neither replica 3 nor 4 were chosen to be the owner
    assert(!updatedToken.data.isOwner(using replicas(3)) &&
      !updatedToken.data.isOwner(using replicas(4)))
  }
}
