package test.rdts.bespoke

import rdts.datatypes.experiments.AutomergyOpGraphLWW.OpGraph

class OpGraphTest extends munit.FunSuite {

  test("basic usage") {

    val empty = OpGraph.bottom[String].empty
    val r1    = empty.set("hi!")

    assertEquals(r1.values, List("hi!"))

    val r2 = empty.set("bye!")

    val r1r2 = r1 `merge` r2

    assertEquals(r1r2.values, List("bye!", "hi!"))

    val deletion = r1r2.del()
    val deleted  = r1r2 `merge` deletion

    assertEquals(deleted.values, List())

    val d2 = deleted `merge` deleted.set("why?")

    assertEquals(d2.values, List("why?"))

    val undoing = deleted.undo(deletion.elements.head._1)

    val undone = d2 `merge` undoing

    // as far as I can tell, the “undone” values should be ordered before the added value (even though it was added later) because the undoing is newer than the additions.
    assertEquals(undone.values, List("bye!", "hi!", "why?"))

  }
}
