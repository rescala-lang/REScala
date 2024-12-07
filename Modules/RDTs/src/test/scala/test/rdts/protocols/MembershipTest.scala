package test.rdts.protocols

import rdts.base.Lattice.syntax
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.{MultiRoundVoting, Paxos, SimpleVoting}

class MembershipTest extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()
  val id4 = LocalUid.gen()

  test("basic membership merge") {
    val membership = Membership.init[Int, Paxos, Paxos](Set(id1, id2, id3).map(_.uid))
    val delta      = membership.upkeep()(using id1)
    println(delta.members)
    val res        = delta `merge` membership
    assertEquals(res, membership)
  }

  test("Membership happy path") {
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

  test("kv membership use") {
    val id1 = Uid.predefined("Node1")
    val id2 = Uid.predefined("Node2")
    val id3 = Uid.predefined("Node3")

    class Replika(val uid: Uid, var mem: Membership[Int, Paxos, Paxos])

    val r1 = Replika(id1, Membership.init(Set(id1, id2, id3)))
    val r2 = Replika(id2, Membership.init(Set(id1, id2, id3)))
    val r3 = Replika(id3, Membership.init(Set(id1, id2, id3)))

    extension (m: Replika)
      def trans(f: LocalUid ?=> Membership[Int, Paxos, Paxos] => Membership[Int, Paxos, Paxos]): Unit = {
        given LocalUid = m.uid.convert
        val delta      = f(m.mem)

        r1.mem = r1.mem `merge` delta
        r2.mem = r2.mem `merge` delta
        r3.mem = r3.mem `merge` delta
      }

    r1.trans(_.write(10))

    var iterations = 0
    while {
      val o1 = r1.mem
      val o2 = r2.mem
      val o3 = r3.mem
      r3.trans(_.upkeep())
      r2.trans(_.upkeep())
      r1.trans(_.upkeep())
      o1 != r1.mem && o2 != r2.mem && o3 != r3.mem
    } do {
      iterations += 1
    }

    assertEquals(iterations, 2)
    assertEquals(r3.mem.log.get(0), Some(10))

  }
}
