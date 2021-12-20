package test.kofre

import org.scalatest.freespec.AnyFreeSpec
import rescala.extra.lattices.Lattice
import rescala.extra.lattices.sequences.{DeltaSequence, Vertex}

class DeltaSequenceCrdtTest extends AnyFreeSpec {

  "basic interaction" in {
    val ds      = DeltaSequence.empty[String]
    val a       = "a"
    val vertex1 = Vertex.fresh()
    val vertex2 = Vertex.fresh()
    val added2 = ds
      .addRight(a, Vertex.start, vertex1, "Hello world!")
      .addRight(a, Vertex.start, vertex2, "Hello world 2!")
    assert(added2.iterator.toList === List("Hello world 2!", "Hello world!"), "add right is correctly ordered")

    val addDelta = added2.addRightDelta(a, vertex2, Vertex.fresh(), "Hello world after 2!")

    val combined = Lattice.merge(added2, addDelta)
    assert(combined.iterator.toList === List("Hello world 2!", "Hello world after 2!", "Hello world!"))

    val addDeltaOtherReplica = added2.addRightDelta(a, vertex2, Vertex.fresh(), "Hello world after 2 the second!")

    val combined2 = Lattice.merge(added2, addDeltaOtherReplica)

    assert(combined2.iterator.toList === List("Hello world 2!", "Hello world after 2 the second!", "Hello world!"))

    val fullCombine  = Lattice.merge(combined, combined2)
    val deltaCombine = Lattice.merge(combined, addDeltaOtherReplica)

    assert(fullCombine.iterator.toList == deltaCombine.iterator.toList)
  }

}
