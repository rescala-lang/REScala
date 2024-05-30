package replication.protocols
import rdts.base.Bottom
import rdts.base.Lattice.merge
import rdts.datatypes.GrowOnlyMap.*
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyMap}
import rdts.dotted.{Dotted, DottedLattice}
import rdts.syntax.LocalUid
import rdts.time.Dots
import replication.protocols.Paxos.*

class PaxosTest extends munit.FunSuite {
  given Bottom[Int] with
    override def empty: Int = Int.MinValue

  given dots: Dots = Dots.empty
  val id1          = LocalUid.gen()
  val id2          = LocalUid.gen()
  val id3          = LocalUid.gen()

  var emptyPaxosObject: Paxos[Int, 2] = Paxos.unchanged(using 2)

  test("Paxos for 3 participants without errors") {
    var a: Paxos[Int, 2] = Paxos.unchanged(using 2)

    a = a merge a.prepare()(using id1)
    a = a merge a.upkeep()(using id1) merge a.upkeep()(using id2) merge a.upkeep()(using id3)
    assertEquals(a.read, None)
    a = a merge a.accept(1)(using id1)
    a = a merge a.upkeep()(using id1) merge a.upkeep()(using id2) merge a.upkeep()(using id3)
    assertEquals(a.read, Some(1))
  }

  test("newer proposal numbers are bigger") {
    var testPaxosObject = emptyPaxosObject

    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
    val firstProposalNumber = testPaxosObject.prepares.head.proposalNumber
    testPaxosObject = testPaxosObject.prepare()(using id1)
    val secondProposalNumber = testPaxosObject.prepares.head.proposalNumber

    assert(firstProposalNumber < secondProposalNumber)
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
    // replica 1 sends accept(1)
    testPaxosObject = testPaxosObject.merge(testPaxosObject.accept(1)(using id1))
    // replica 2 and 3 recieve accept
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.upkeep()(using id2)).merge(testPaxosObject.upkeep()(using id3))
    // replica 1 receives accepted
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1))
    // replica 2 sends prepare to propose a new value
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id2))
    // replica 1 receives 2's new prepare, get the value in 1's promise
    val promiseValue = testPaxosObject.upkeep()(using id1).promises.head.value.get
    // assert the promise contains the previously accepted value
    assertEquals(promiseValue, 1)
  }

  test("acceptor sends promise with highest proposal number") {
    var testPaxosObject = emptyPaxosObject

    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
      .merge(testPaxosObject.prepare()(using id2))
      .merge(testPaxosObject.prepare()(using id3))
    val highestProposalNumber = testPaxosObject.prepares.maxBy(_.proposalNumber).proposalNumber
    testPaxosObject = testPaxosObject.upkeep()(using id1)
    val promiseProposalNumber = testPaxosObject.promises.head.proposalNumber

    assertEquals(promiseProposalNumber, highestProposalNumber)
  }

  test("accept contains value of promise with highest proposal number") {
    assert(true)
    var testPaxosObject = emptyPaxosObject
    // replica 1 sends prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id1))
    // 1, 2 and 3 receive prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    // 1 sends accept
    testPaxosObject = testPaxosObject.merge(testPaxosObject.accept(1)(using id1))
    // 1,2 and 3 receive accept
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
    id2)).merge(testPaxosObject.upkeep()(using id3))
    // replica 2 sends prepare
    testPaxosObject = testPaxosObject.merge(testPaxosObject.prepare()(using id2))
    // 1 and 2 receives 2's prepare
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using id2))
    // 2 sends accept, which should contain the value of 1's promise and not the value "2"
    val acceptValue = testPaxosObject.accept(2)(using id2).accepts.head.value
    // assert that the value in 2's accept message is the value of 1's promise
    assertEquals(acceptValue, 1)
  }
}
