package replication.example

import channels.SynchronousLocalConnection
import rdts.base.LocalUid
import replication.{DeltaDissemination, ProtocolMessage}

class DeltaDisseminationTest extends munit.FunSuite {
  test("basics") {

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2 = DeltaDissemination[Set[String]](LocalUid.gen(), _ => ())

    val sync = SynchronousLocalConnection[ProtocolMessage[Set[String]]]()

    dd2.addLatentConnection(sync.client)
    dd1.addLatentConnection(sync.server)

    dd1.pingAll()
    dd2.pingAll()

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))

    assertEquals(dd1.allDeltas.toSet, dd2.allDeltas.toSet)

  }
}
