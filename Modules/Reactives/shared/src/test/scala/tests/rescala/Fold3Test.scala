package tests.rescala

import tests.rescala.testtools.FunSuiteInvertedAssert

class Fold3Test extends FunSuiteInvertedAssert {
  import reactives.default.*
  {

    test("scala 3 fold expressions") {

      val word  = Evt[String]()
      val count = Evt[Int]()
      val reset = Evt[Unit]()

      val resetB = reset branch (_ => "")

      val wordB  = word branch identity
      val countB = count branch (Fold.current * _)

      val res = Fold("")(resetB, wordB, countB)

      assert(res.readValueOnce == "")
      count.fire(10)
      assert(res.readValueOnce == "")
      reset.fire()
      assert(res.readValueOnce == "")
      word.fire("hello")
      assert(res.readValueOnce == "hello")
      count.fire(2)
      assert(res.readValueOnce == "hellohello")
      word.fire("world")
      assert(res.readValueOnce == "world")
      transaction(count, word, reset) {
        count.admit(2)
        word.admit("do them all!")
        reset.admit(())

      }
      assert(res.readValueOnce == "do them all!do them all!")
    }

  }
}
