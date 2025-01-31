package tests.rescala

import munit.FunSuite

class Fold3Test extends FunSuite {

  import reactives.default.*
  {

    test("scala 3 fold expressions") {

      val word = Evt[String]()
      val count = Evt[Int]()
      val reset = Evt[Unit]()

      val resetB = reset `branch` (_ => "")

      val wordB = word `branch` identity
      val countB = count `branch` (Fold.current * _)

      val res = Fold("")(resetB, wordB, countB)

      assertEquals(res.readValueOnce, "")
      count.fire(10)
      assertEquals(res.readValueOnce, "")
      reset.fire()
      assertEquals(res.readValueOnce, "")
      word.fire("hello")
      assertEquals(res.readValueOnce, "hello")
      count.fire(2)
      assertEquals(res.readValueOnce, "hellohello")
      word.fire("world")
      assertEquals(res.readValueOnce, "world")
      transaction(count, word, reset) {
        count.admit(2)
        word.admit("do them all!")
        reset.admit(())

      }
      assertEquals(res.readValueOnce, "do them all!do them all!")
    }

  }
}
