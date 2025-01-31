package tests.rescala.misc

class RecurringPropagation extends munit.FunSuite {

  import reactives.default.*
  {

    test("can start propagation in observers") {

      val e1 = Evt[Int]()
      val m1 = e1.map(_ + 10)
      val v1 = Var(2)

      m1.observe(v1.set)

      assertEquals(v1.readValueOnce, 2)

      e1.fire(100)
      assertEquals(v1.readValueOnce, 110)

    }

    test("recursive propagation") {

      val e1 = Evt[Int]()
      val m1 = e1.map(_ + 10)
      val v1 = Var(2)

      m1.observe(v1.set)

      assertEquals(v1.readValueOnce, 2)

      v1.observe(current => if current > 100 then () else e1.fire(current))

      assertEquals(v1.readValueOnce, 102)

    }

  }
}
