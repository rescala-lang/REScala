package test.rdts.protocols

import rdts.base.LocalUid
import rdts.datatypes.experiments.protocols.{LogHack, Membership}
import rdts.datatypes.experiments.protocols.simplified.{MultiRoundVoting, Paxos, SimpleVoting}

class SimpleMembershipTests extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()

  test("Membership happy path") {
    given LogHack(false)
    var membership = Membership.init[Int, Paxos, Paxos](Set(id1, id2, id3).map(_.uid))
    // id1 writes -> prepare
    membership = membership.merge(membership.write(1)(using id1))
    assert(membership.isMember(using id1))
    assert(membership.isMember(using id2))
    assert(membership.isMember(using id3))
    // all upkeep --> promise and enter phase 2
    membership = membership
      .merge(membership.upkeep()(using id1))
      .merge(membership.upkeep()(using id2))
      .merge(membership.upkeep()(using id3))
    // all upkeep again -> propose value
    membership = membership
      .merge(membership.upkeep()(using id1))
      .merge(membership.upkeep()(using id2))
      .merge(membership.upkeep()(using id3))
    // all upkeep again -> accept value
    membership = membership
      .merge(membership.upkeep()(using id1))
      .merge(membership.upkeep()(using id2))
      .merge(membership.upkeep()(using id3))
    assertEquals(membership.currentMembers, Set(id1,id2,id3).map(_.uid))
    assertEquals(membership.log, List(1))
  }
}
