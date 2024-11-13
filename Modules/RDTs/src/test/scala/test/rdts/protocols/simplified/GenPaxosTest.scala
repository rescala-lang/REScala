package test.rdts.protocols.simplified

import rdts.base.LocalUid
import rdts.datatypes.experiments.protocols.Participants
import rdts.datatypes.experiments.protocols.simplified.GeneralizedPaxos
import rdts.datatypes.experiments.protocols.simplified.GeneralizedPaxos.given

class GenPaxosTest extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()

  given Participants = Participants(Set(id1, id2, id3).map(_.uid))

  val emptyPaxosObject: GeneralizedPaxos[Int] = GeneralizedPaxos()
  test("write works as expected") {
    var testPaxosObject = emptyPaxosObject
    val writeValue      = 1
    // replica 1 tries to write
    testPaxosObject = testPaxosObject.merge(testPaxosObject.write(writeValue)(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
      id2
    )).merge(testPaxosObject.upkeep()(using id3))
    assertEquals(testPaxosObject.read, None)
    // replica 1 tries to write again
    testPaxosObject = testPaxosObject.merge(testPaxosObject.write(writeValue)(using id1))
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
      id2
    )).merge(testPaxosObject.upkeep()(using id3))
    assertEquals(testPaxosObject.read, Some(writeValue))
  }

  test("concurrent writes") {
    var testPaxosObject = emptyPaxosObject
    // replica 1 and 2 try to write
    testPaxosObject =
      testPaxosObject.merge(testPaxosObject.write(1)(using id1)).merge(testPaxosObject.write(2)(using id2))
    // deliver prepares
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
      id2
    )).merge(testPaxosObject.upkeep()(using id3))
    assertEquals(testPaxosObject.read, None)
    // deliver proposal
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
      id2
    )).merge(testPaxosObject.upkeep()(using id3))
    // deliver accepted
    testPaxosObject = testPaxosObject.merge(testPaxosObject.upkeep()(using id1)).merge(testPaxosObject.upkeep()(using
      id2
    )).merge(testPaxosObject.upkeep()(using id3))
    assert(clue(testPaxosObject.read) == Some(2) || clue(testPaxosObject.read) == Some(1))
  }
}
