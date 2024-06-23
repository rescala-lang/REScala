package test.rdts.containers

import rdts.base.{Bottom, LocalUid}
import rdts.datatypes.contextual.EnableWinsFlag
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

class DeltaBufferDottedTest extends munit.FunSuite {

  test("basic interaction") {

    given LocalUid = LocalUid.gen()

    val dbe = DeltaBuffer[Dotted[EnableWinsFlag]](Dotted(EnableWinsFlag.empty))

    assertEquals(dbe.state.data, Bottom.empty[EnableWinsFlag])
    assert(!dbe.data.read)
    assertEquals(dbe.deltaBuffer, List.empty)

    val dis = dbe.modd(_.enable()).modd(_.enable())
    assert(dis.data.read)
    val en = dis.modd(_.disable())

    assert(!en.data.read)
    assertEquals(en.deltaBuffer.size, 3)

  }

}
