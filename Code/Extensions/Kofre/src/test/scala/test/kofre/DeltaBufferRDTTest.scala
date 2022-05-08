package test.kofre

import kofre.base.{Bottom, Defs, Lattice}
import kofre.decompose.containers.DeltaBufferRDT
import kofre.decompose.interfaces.EnableWinsFlag
import kofre.rga.{DeltaSequence, Vertex}
import org.scalatest.freespec.AnyFreeSpec

class DeltaBufferRDTTest extends AnyFreeSpec {

  "basic interaction" in {

    val dbe = DeltaBufferRDT[EnableWinsFlag](Defs.genId())

    assert(dbe.state === Bottom.empty[EnableWinsFlag])
    assert(!dbe.read)
    assert(dbe.deltaBuffer === List.empty)

    val dbe2 = dbe.enable().enable().disable()

    assert(dbe2.read)
    assert(dbe2.deltaBuffer.size == 2)

  }

}
