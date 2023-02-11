package tests.rescala.misc

import tests.rescala.testtools.RETests

class ScheduledSetting extends RETests {
  multiEngined { engine =>
    import engine._

    test("setting during inner event") {

      val outer  = Var("outer")
      val source = Var[Signal[String]](outer)
      val flat   = source.flatten

      val evt = Evt[Unit]()

      val changes = evt.map { _ =>
        val inner = Var("inner")
        source.set(inner)
        outer.set("changed outer")
        (flat.value, outer.value)
      }.list()

      evt.fire()

      assert(flat.now === "inner")

      assert(changes.now == List(("outer", "outer")))

    }

  }
}
