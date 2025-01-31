package tests.rescala.dynamic

import munit.FunSuite

class Toggle extends FunSuite {

  import reactives.default.*
  {

    /* toggle */
    test("toggle the Initial Value Is Set Correctly") {
      val e  = Evt[Int]()
      val v1 = Var(1)
      val s1 = v1.map { _ + 1 }
      val v2 = Var(11)
      val s2 = v2.map { _ + 1 }
      val s  = e.toggle(s1, s2)

      assertEquals(s.readValueOnce, 2)
      assertEquals(s2.readValueOnce, 12)
    }

    test("toggle the Event Switches The Signal") {
      val e  = Evt[Int]()
      val v1 = Var(1)
      val s1 = v1.map { _ + 1 }
      val v2 = Var(11)
      val s2 = v2.map { _ + 1 }
      val s  = e.toggle(s1, s2)

      assertEquals(s.readValueOnce, 2)
      e.fire(1)
      assertEquals(s.readValueOnce, 12)
      v2.set(12)
      assertEquals(s.readValueOnce, 13)
      v1.set(2)
      assertEquals(s.readValueOnce, 13)
      e.fire(1)
      v1.set(3)
      assertEquals(s.readValueOnce, 4)
      v2.set(13)
      assertEquals(s.readValueOnce, 4)

    }

  }
}
