package test.kofre

import kofre.base.{Bottom, Defs, Lattice}
import kofre.datatypes.EnableWinsFlag
import kofre.decompose.containers.DeltaBufferRDT

class DeltaBufferRDTTest extends munit.FunSuite {

  test("basic interaction") {

    val dbe = DeltaBufferRDT[EnableWinsFlag](Defs.genId())

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
