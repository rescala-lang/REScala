package replication.example

import channels.SynchronousLocalConnection
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.LocalUid
import replication.{DeltaDissemination, ProtocolMessage}

class DeltaDisseminationTest extends munit.FunSuite {
  test("basics") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2, dd3 = DeltaDissemination[Set[String]](LocalUid.gen(), _ => (), None, true)

    val sync = SynchronousLocalConnection[ProtocolMessage[Set[String]]]()

    dd2.addObjectConnection(sync.client("2"))
    dd1.addObjectConnection(sync.server)
    dd3.addObjectConnection(sync.client("3"))

    dd1.pingAll()
    dd2.pingAll()
    dd3.pingAll()

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))
    dd3.applyDelta(Set("c"))

    assertEquals(dd1.allPayloads.map(_.payload.data).toSet, dd2.allPayloads.map(_.payload.data).toSet)
    assertEquals(dd2.allPayloads.map(_.payload.data).toSet, dd3.allPayloads.map(_.payload.data).toSet)

  }

  test("long ???") {

    given JsonValueCodec[Long] = JsonCodecMaker.make

    writeToArray(1405345091900L)

    assert(true)

  }
}
