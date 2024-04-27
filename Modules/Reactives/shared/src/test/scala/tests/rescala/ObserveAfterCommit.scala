package tests.rescala.misc

import reactives.default.*

class ObserveAfterCommit extends munit.FunSuite {

  test("observe after TX throws") {

    val v1 = Var(1)

    // TODO: the macro currently does not replace the dynamic scope found during the .set call.
    // That is potentially a bit weird, but semantically pretty reasonable (the set always looks in the dynamic scope)
    val next: Signal[() => Unit] = v1.map { i => () => v1.set(i + 1) }

    val callback = next.now

    assertEquals(v1.now, 1)

    intercept[IllegalStateException](callback())

    assertEquals(v1.now, 1)

  }

}
