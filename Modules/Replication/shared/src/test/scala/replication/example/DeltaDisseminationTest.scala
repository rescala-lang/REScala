package replication.example

import channels.SynchronousLocalConnection
import rdts.base.LocalUid
import replication.{DeltaDissemination, ProtocolMessage}

class DeltaDisseminationTest extends munit.FunSuite {
  test("basics") {

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2, dd3 = DeltaDissemination[Set[String]](LocalUid.gen(), _ => (), None, true)(using null)

    val sync = SynchronousLocalConnection[ProtocolMessage[Set[String]]]()

    dd2.addLatentConnection(sync.client("2"))
    dd1.addLatentConnection(sync.server)
    dd3.addLatentConnection(sync.client("3"))

    dd1.pingAll()
    dd2.pingAll()
    dd3.pingAll()

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))
    dd3.applyDelta(Set("c"))

    assertEquals(dd1.allPayloads.map(_.payload.data).toSet, dd2.allPayloads.map(_.payload.data).toSet)
    assertEquals(dd2.allPayloads.map(_.payload.data).toSet, dd3.allPayloads.map(_.payload.data).toSet)

  }
}
