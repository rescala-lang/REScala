package tests.rescala.static.conversions

import scala.collection.LinearSeq

class FoldTests extends munit.FunSuite {

  import reactives.default.*
  {

    /* fold */
    test("fold the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val f              = (x: Int, y: Int) => x + y
      val s: Signal[Int] = e.fold(10)(f)
      assertEquals(s.readValueOnce, 10)
    }

    test("fold the Result Signal Increases When Events Occur") {
      val e              = Evt[Int]()
      val f              = (x: Int, y: Int) => x + y
      val s: Signal[Int] = e.fold(10)(f)
      e.fire(1)
      e.fire(1)
      assertEquals(s.readValueOnce, 12)
    }

    /* count */
    test("count the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()
      assertEquals(s.readValueOnce, 0)
    }

    test("count the Result Signal Increases When Events Occur") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()
      e.fire(1)
      e.fire(1)
      assertEquals(s.readValueOnce, 2)
    }

    /* iterate */
    test("iterate the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val f              = (x: Int) => x
      val s: Signal[Int] = e.iterate(10)(f)
      assertEquals(s.readValueOnce, 10)
    }

    test("iterate the Function is Executed Every Time The Event Fires") {
      var test: Int      = 0
      val e              = Evt[Int]()
      val f              = (x: Int) => { test += 1; x }
      val s: Signal[Int] = e.iterate(10)(f)
      e.fire(1)
      assertEquals(test, 1)
      e.fire(2)
      assertEquals(test, 2)
      e.fire(1)
      assertEquals(test, 3)
      assertEquals(s.readValueOnce, 10)
    }

    // TODO: does it make sense ?
    test("iterate the Parameter Is Always The Init Value") {
      var test: Int      = 0
      val e              = Evt[Int]()
      val f              = (x: Int) => { test = x; x + 1 }
      val s: Signal[Int] = e.iterate(10)(f)
      e.fire(1)
      assertEquals(test, 10)
      e.fire(2)
      assertEquals(test, 11)
      e.fire(1)
      assertEquals(test, 12)
      assertEquals(s.readValueOnce, 13)
    }

    test("iterate the result signal does not depend on the event value") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.iterate(10)(identity)
      e.fire(1)
      assertEquals(s.readValueOnce, 10)
      e.fire(2)
      assertEquals(s.readValueOnce, 10)
      e.fire(1)
      assertEquals(s.readValueOnce, 10)
    }

    /* latest */
    test("latest the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.hold(10)

      assertEquals(s.readValueOnce, 10)
    }

    test("latest the Function is Executed Every Time The Event Fires") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.hold(10)

      e.fire(1)
      assertEquals(s.readValueOnce, 1)
      e.fire(2)
      assertEquals(s.readValueOnce, 2)
      e.fire(1)
      assertEquals(s.readValueOnce, 1)
    }

    /* latestOption */
    test("latest Option the Initial Value Is Set Correctly") {
      val e                      = Evt[Int]()
      val s: Signal[Option[Int]] = e.holdOption()

      assertEquals(s.readValueOnce, None)
    }

    test("latest Option the Function is Executed Every Time The Event Fires") {
      val e                      = Evt[Int]()
      val s: Signal[Option[Int]] = e.holdOption()

      e.fire(1)
      assertEquals(s.readValueOnce, Option(1))
      e.fire(2)
      assertEquals(s.readValueOnce, Option(2))
      e.fire(1)
      assertEquals(s.readValueOnce, Option(1))
    }

    /* last */
    test("last the Initial Value Is Set Correctly") {
      val e                         = Evt[Int]()
      val s: Signal[LinearSeq[Int]] = e.list(5)

      assertEquals(s.readValueOnce, List())
    }

    test("last collects The LastN Events") {
      val e                         = Evt[Int]()
      val s: Signal[LinearSeq[Int]] = e.list(5)

      assertEquals(s.readValueOnce, LinearSeq())
      e.fire(1)
      assertEquals(s.readValueOnce, LinearSeq(1))
      e.fire(2)
      assertEquals(s.readValueOnce, LinearSeq(1, 2))

      e.fire(3)
      e.fire(4)
      e.fire(5)
      assertEquals(s.readValueOnce, LinearSeq(1, 2, 3, 4, 5))
      e.fire(6)
      assertEquals(s.readValueOnce, LinearSeq(2, 3, 4, 5, 6))
    }

    /* list */
    test("list the Initial Value Is Set Correctly") {
      val e = Evt[Int]()
      val s = e.list()

      assertEquals(s.readValueOnce, List())
    }

    test("list the Function is Executed Every Time The Event Fires") {
      val e = Evt[Int]()
      val s = e.list()

      assertEquals(s.readValueOnce, List())
      e.fire(1)
      assertEquals(s.readValueOnce, List(1))
      e.fire(2)
      assertEquals(s.readValueOnce, List(2, 1))

      e.fire(3)
      e.fire(4)
      e.fire(5)
      e.fire(6)
      assertEquals(s.readValueOnce, List(6, 5, 4, 3, 2, 1))
    }

    test("create folds during tx") {

      val e = Evt[String]()
      val listed = transaction(e) { t ?=>
        e.admit("hello")
        e.list()
      }

      assertEquals(listed.readValueOnce, List("hello"))

    }

    /* fold expressions */

    test("fold expression works") {

      val word  = Evt[String]()
      val count = Evt[Int]()
      val reset = Evt[Unit]()
      val res = Fold("")(
        reset `branch` (_ => ""),
        word `branch` identity,
        count `branch` (Fold.current * _)
      )

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
      transaction(count, word, reset) { at ?=>
        count.admit(2)
        word.admit("do them all!")
        reset.admit(())

      }
      assertEquals(res.readValueOnce, "do them all!do them all!")
    }

    test("fold expression compiles with values of a subtype") {
      val e0 = Evt[Unit]()
      val e1 = Evt[Int]()
      val res = Fold(Option.empty[Int])(
        e0 `branch` { _ => Some(1) },
        e1 `branch` { _ => Option(2) }
      )

      assertEquals(res.readValueOnce, None)
      e0.fire()
      assertEquals(res.readValueOnce, Some(1))
      e1.fire(0)
      assertEquals(res.readValueOnce, Some(2))
    }

    // test("changing only a signal when folding") {
    //  val e = Evt[Int]()
    //  val v = Var(0)
    //  val f = e.fold(0) { (_, _) => v.value }
    //  f observe identity
    //
    //  v.set(1)
    // }

  }
}
