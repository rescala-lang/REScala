package tests.rescala.misc

import munit.FunSuite

class DisconnectTests extends FunSuite {

  import reactives.default.*
  {

    test("remove incoming dependencies when disconnecting signals") {
      val v1 = Var(10)
      val m1 = v1.map(_ + 10)
      val m2 = m1.map(_ + 10)

      assertEquals(v1.readValueOnce, 10)
      assertEquals(m1.readValueOnce, 20)
      assertEquals(m2.readValueOnce, 30)

      v1.set(12)
      assertEquals(v1.readValueOnce, 12)
      assertEquals(m1.readValueOnce, 22)
      assertEquals(m2.readValueOnce, 32)

      m1.disconnect()

      v1.set(16)
      assertEquals(v1.readValueOnce, 16)
      assertEquals(m1.readValueOnce, 22)
      assertEquals(m2.readValueOnce, 32)

    }

  }
}
