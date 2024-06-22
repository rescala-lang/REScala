package test.rdts.containers

import rdts.base.{Bottom, Uid}
import rdts.datatypes.contextual.EnableWinsFlag
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, LocalUid}

class DeltaBufferDottedTest extends munit.FunSuite {

  test("basic interaction") {

    given LocalUid = Uid.gen()

    val dbe = DeltaBuffer[Dotted[EnableWinsFlag]](Dotted(EnableWinsFlag.empty))

    assertEquals(dbe.state.data, Bottom.empty[EnableWinsFlag])
    assert(!dbe.data.read)
    assertEquals(dbe.deltaBuffer, List.empty)

    val dis = dbe.mod(_.enable()).mod(_.enable())
    assert(dis.data.read)
    val en = dis.mod(_.disable())

    assert(!en.data.read)
    assertEquals(en.deltaBuffer.size, 3)

  }

}
