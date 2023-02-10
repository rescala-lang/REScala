package test.kofre

import kofre.base.{Bottom, Id, Lattice}
import kofre.datatypes.EnableWinsFlag
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, ReplicaId}

class DeltaBufferDottedTest extends munit.FunSuite {

  test("basic interaction") {

    given ReplicaId = Id.gen()

    val dbe = DeltaBuffer[Dotted[EnableWinsFlag]](Dotted(EnableWinsFlag.empty))

    assertEquals(dbe.state.store, Bottom.empty[EnableWinsFlag])
    assert(!dbe.read)
    assertEquals(dbe.deltaBuffer, List.empty)

    val dis = dbe.enable().enable()
    assert(dis.read)
    val en = dis.disable()

    assert(!en.read)
    assertEquals(en.deltaBuffer.size, 3)

  }

}
