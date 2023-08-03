package test.kofre.bespoke
import kofre.datatypes.contextual.ReplicatedList
import test.kofre.idFromString
import kofre.base.Uid
import kofre.dotted.Dotted

class ReplicatedListTest extends munit.FunSuite {

  test("AuctionData can be in Dotted") {
    val v1 = Dotted(ReplicatedList.empty[String])

    val v2 = v1.insert(using Uid.predefined("a"))(0, "10")

    assertEquals(v2.toList, List("10"))

    val v3 = v2.insert(using Uid.predefined("a"))(1, "20")

    assertEquals(v3.toList, List("10", "20"))

    val v4 = v3.insert(using Uid.predefined("a"))(1, "30")

    assertEquals(v4.toList, List( "10", "30", "20"))

  }

}
