package tests.rescala.misc

import tests.rescala.testtools.RETests

class GlitchFreedomTestSuite extends RETests {
  multiEngined { engine =>
    import engine.*

    test("no Glitches In Simple Case") {

      val v1 = Var(1)
      val s1 = v1.map { 2 * _ }
      val s2 = v1.map { 3 * _ }
      val s3 = Signal.lift(s1, s2) { _ + _ }

      val s1List = s1.changed.list()
      val s2List = s2.changed.list()
      val s3List = s3.changed.list()

      v1.set(3)

      assertEquals(s1List.readValueOnce, List(6))
      assertEquals(s2List.readValueOnce, List(9))
      assertEquals(s3List.readValueOnce, List(15))

    }

  }
}
