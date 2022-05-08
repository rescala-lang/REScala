package test.kofre

import kofre.base.{Bottom, Defs, Lattice}
import kofre.decompose.containers.DeltaBufferRDT
import kofre.decompose.interfaces.EnableWinsFlag.EWFlagPlain
import kofre.decompose.interfaces.EnableWinsFlag.EnableWinsFlagOps
import kofre.rga.{DeltaSequence, Vertex}
import org.scalatest.freespec.AnyFreeSpec

class DeltaBufferRDTTest extends AnyFreeSpec {

  "basic interaction" in {

    val dbe = DeltaBufferRDT[EWFlagPlain](Defs.genId())

    assert(dbe.state === Bottom.empty[EWFlagPlain])
    assert(!dbe.read)
    assert(dbe.deltaBuffer === List.empty)

    val dbe2 = dbe.enable().enable().disable()

    assert(dbe2.read)
    assert(dbe2.deltaBuffer.size == 2)

    println(s"${dbe2.state}\n${dbe2.deltaBuffer}")


  }

}
