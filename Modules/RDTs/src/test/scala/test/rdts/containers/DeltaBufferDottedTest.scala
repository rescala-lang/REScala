package test.rdts.containers

import rdts.base.{Bottom, Uid}
import rdts.datatypes.contextual.EnableWinsFlag
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, LocalReplicaId}

class DeltaBufferDottedTest extends munit.FunSuite {

  test("basic interaction") {

    given LocalReplicaId = Uid.gen()

    val dbe = DeltaBuffer[Dotted[EnableWinsFlag]](Dotted(EnableWinsFlag.empty))

    assertEquals(dbe.state.data, Bottom.empty[EnableWinsFlag])
    assert(!dbe.read)
    assertEquals(dbe.deltaBuffer, List.empty)

    val dis = dbe.enable().enable()
    assert(dis.read)
    val en = dis.disable()

    assert(!en.read)
    assertEquals(en.deltaBuffer.size, 3)

  }

}
