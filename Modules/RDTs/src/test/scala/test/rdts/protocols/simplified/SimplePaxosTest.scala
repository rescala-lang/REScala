package test.rdts.protocols.simplified

import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.simplified.Paxos
import rdts.datatypes.experiments.protocols.simplified.Paxos.{*, given}
import rdts.time.Dots

import scala.math.Ordering.Implicits.infixOrderingOps

class SimplePaxosTest extends munit.FunSuite {
  given Bottom[Int] with
    override def empty: Int = Int.MinValue

  given dots: Dots = Dots.empty
  val id1          = LocalUid.gen()
  val id2          = LocalUid.gen()
  val id3          = LocalUid.gen()

  given members: Set[Uid] = Set(id1, id2, id3).map(_.uid)

  var emptyPaxosObject: Paxos[Int] = Paxos.init(members)

//  test("Merge fails with different members") {
//    val p1: Paxos[Int] = Paxos.unchanged.copy(members = Set(id1).map(_.uid))
//    val p2: Paxos[Int] = Paxos.unchanged.copy(members = Set(id2).map(_.uid))
//    interceptMessage[IllegalArgumentException](
//      "requirement failed: cannot merge two Paxos instances with differing members"
//    ) {
//      p1 `merge` p2
//    }
//  }

  test("Paxos for 3 participants without errors") {
    var a: Paxos[Int] = emptyPaxosObject

    val proposal = a.chooseProposalNumber(using id1)
    a = a `merge` a.prepare()(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.read, None)
    a = a `merge` a.propose(proposal, 1)(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.read, Some(1))
  }

  test("newer proposal numbers are bigger") {
    var testPaxosObject = emptyPaxosObject

    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
    val firstProposalNumber  = testPaxosObject.prepares.head.proposal
    val secondProposalNumber = testPaxosObject.chooseProposalNumber(using id1)

    assert(firstProposalNumber < secondProposalNumber)
  }

  test("prepare does not change members") {
    var testPaxosObject = emptyPaxosObject

    assert(testPaxosObject.members.nonEmpty)
    assertEquals(testPaxosObject.members, testPaxosObject.prepare()(using id1).members)
  }

  test("write does not change members") {
    var testPaxosObject = emptyPaxosObject

    assert(testPaxosObject.members.nonEmpty)

    val afterwrite = testPaxosObject.write(5)(using id1)
    assertEquals(afterwrite.members.keySet, testPaxosObject.members.keySet)
  }

  test("No changes for older proposals") {
    var testPaxosObject1 = emptyPaxosObject
    // replica 1 sends prepare
    testPaxosObject1 = testPaxosObject1.merge(testPaxosObject1.prepare()(using id1))
    // replica 2 sends prepare
    testPaxosObject1 = testPaxosObject1.merge(testPaxosObject1.prepare()(using id2))
    testPaxosObject1 =
      testPaxosObject1.merge(testPaxosObject1.upkeep()(using id1)).merge(testPaxosObject1.upkeep()(using id2))
    var testPaxosObject2 = emptyPaxosObject
    // replica 3 sends prepare, with smaller proposal number
    testPaxosObject2 = testPaxosObject2.merge(testPaxosObject2.prepare()(using id3))
    // replica 1 receives 3's prepare
    testPaxosObject1 = testPaxosObject1.merge(testPaxosObject2)
    // assert that the object before and after replica 1 calls upkeep is the same
    assertEquals(testPaxosObject1, testPaxosObject1.merge(testPaxosObject1.upkeep()(using id1)))
  }

  test("promise sends previously accepted value") {
    var testPaxosObject = emptyPaxosObject

    // replica 1 sends prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
    // replica 2 and 3 receive prepare
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.upkeep()(using id2)).merge(testPaxosObject.upkeep()(using id3))
    // replica 1 receives 2 and 3's promise
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1))
    // replica 1 sends propose(1)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.propose(1)(using id1))
    // replica 2 and 3 receive propose
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.upkeep()(using id2)).merge(testPaxosObject.upkeep()(using id3))
    // replica 1 receives accepted
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1))
    // replica 2 sends prepare to propose a new value
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id2))
    // replica 1 receives 2's new prepare, get the value in 1's promise

    val promiseValue = testPaxosObject.upkeep()(using id1).promises.maxBy(_.proposal).highestAccepted.get._2
    // assert the promise contains the previously accepted value
    assertEquals(promiseValue, 1)
  }

  test("acceptor sends promise with highest proposal number") {
    var testPaxosObject = emptyPaxosObject

    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
      .merge(testPaxosObject.prepare()(using id2))
      .merge(testPaxosObject.prepare()(using id3))
    val highestProposalNumber = testPaxosObject.prepares.map(_.proposal).max
    testPaxosObject = testPaxosObject.upkeep()(using id1)
    val promiseProposalNumber = testPaxosObject.promises.head.proposal

    assertEquals(promiseProposalNumber, highestProposalNumber)
  }

  test("accept contains value of promise with highest proposal number") {
    var testPaxosObject = emptyPaxosObject
    // replica 1 sends prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
    // 1, 2 and 3 receive prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    // 1 sends accept
    testPaxosObject = testPaxosObject.merge(testPaxosObject.propose(1)(using id1))
    // 1,2 and 3 receive accept
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    // replica 2 sends prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id2))
    // 1 and 2 receives 2's prepare
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using id2))
    // 2 sends accept, which should contain the value of 1's promise and not the value "2"
    val acceptValue = testPaxosObject.propose(2)(using id2).accepts.head.value
    // assert that the value in 2's accept message is the value of 1's promise
    assertEquals(acceptValue, 1)
  }

  test("write works as expected") {
    var testPaxosObject = emptyPaxosObject
    val writeValue      = 1
    // replica 1 tries to write
    testPaxosObject = testPaxosObject.merge(testPaxosObject.write(writeValue)(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    assertEquals(testPaxosObject.read, None)
    // replica 1 tries to write again
    testPaxosObject = testPaxosObject.merge(testPaxosObject.write(writeValue)(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    assertEquals(testPaxosObject.read, Some(writeValue))
  }

  test("concurrent writes") {
    var testPaxosObject = emptyPaxosObject
    // replica 1 and 2 try to write
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.write(1)(using id1)).merge(testPaxosObject.write(2)(using id2))
    // deliver prepares
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    assertEquals(testPaxosObject.read, None)
    // deliver proposal
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    // deliver accepted
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    assert(clue(testPaxosObject.read) == Some(2) || clue(testPaxosObject.read) == Some(1))
  }
}
