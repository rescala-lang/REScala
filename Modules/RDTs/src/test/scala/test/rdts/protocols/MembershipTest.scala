package test.rdts.protocols

import rdts.base.LocalUid
import rdts.datatypes.experiments.protocols.simplified.{MultiRoundVoting, Paxos, SimpleVoting}
import rdts.datatypes.experiments.protocols.{LogHack, Membership}

class MembershipTest extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()
  val id4 = LocalUid.gen()

  test("Membership happy path") {
    given LogHack(false)
    var membership = Membership.init[Int, Paxos, Paxos](Set(id1, id2, id3).map(_.uid))
    // id1 writes -> prepare
    assert(membership.isMember(using id1))
    membership = membership.merge(membership.write(1)(using id1))
    assert(membership.isMember(using id1))
    assert(membership.isMember(using id2))
    assert(membership.isMember(using id3))

    def doUpkeeps() = {
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
    }

    doUpkeeps()

    assertEquals(membership.currentMembers, Set(id1, id2, id3).map(_.uid))
    assertEquals(membership.read, List(1))

    membership = membership.merge(membership.write(2)(using id2))

    doUpkeeps()

    assertEquals(membership.read, List(1, 2))
  }

  test("Membership with member change") {
    given LogHack(false)
    var membership = Membership.init[Int, Paxos, Paxos](Set(id1, id2, id3).map(_.uid))
    // id1 writes -> prepare
    assert(membership.isMember(using id1))
    membership = membership.merge(membership.write(1)(using id1))
    assert(membership.isMember(using id1))
    assert(membership.isMember(using id2))
    assert(membership.isMember(using id3))
    // id2 vote kicks id1
    membership = membership.merge(membership.removeMember(id1.uid)(using id2))
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
    assertEquals(membership.currentMembers, Set(id2, id3).map(_.uid))
    assertEquals(membership.read, List())
    // non-members should not be able to modify consensus
    assertEquals(
      membership.merge(membership.write(1)(using id1)),
      membership
    )
    assertNotEquals(
      membership.merge(membership.write(1)(using id2)),
      membership
    )
    assertNotEquals(
      membership.merge(membership.write(1)(using id3)),
      membership
    )
  }

  test("Fixed counterexample from suite") {
    given LogHack(false)
    var membership1 = Membership.init[Int, Paxos, Paxos](Set(id1, id2, id3, id4).map(_.uid))
    var membership2 = Membership.init[Int, Paxos, Paxos](Set(id1, id2, id3, id4).map(_.uid))
    // id1 writes -> prepare
    membership1 = membership1.merge(membership1.write(1)(using id1))
    // upkeep on same id
    membership1 = membership1.merge(membership1.upkeep()(using id1))
    // write on id2
    membership2 = membership2.merge(membership2.write(2)(using id2))
    // upkeep on id2
    membership2 = membership2.merge(membership2.upkeep()(using id2))
    // merge id1 and id2
    membership1 = membership1.merge(membership2)
    membership2 = membership2.merge(membership1)
    // -> logs should diverge
    assertEquals(membership1.log, membership2.log)
  }
}
