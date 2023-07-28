package test.kofre

import kofre.base.{Bottom, Uid}
import kofre.datatypes.contextual.EnableWinsFlag
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, ReplicaId}

class DeltaBufferDottedTest extends munit.FunSuite {

  test("basic interaction") {

    given ReplicaId = Uid.gen()

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
