package tests.rescala.static.conversions

import tests.rescala.testtools.RETests

import scala.collection.LinearSeq

class FoldTests extends RETests {
  multiEngined { engine =>
    import engine.*

    /* fold */
    test("fold the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val f              = (x: Int, y: Int) => x + y
      val s: Signal[Int] = e.fold(10)(f)
      assert(s.readValueOnce == 10)
    }

    test("fold the Result Signal Increases When Events Occur") {
      val e              = Evt[Int]()
      val f              = (x: Int, y: Int) => x + y
      val s: Signal[Int] = e.fold(10)(f)
      e.fire(1)
      e.fire(1)
      assert(s.readValueOnce == 12)
    }

    /* count */
    test("count the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()
      assert(s.readValueOnce == 0)
    }

    test("count the Result Signal Increases When Events Occur") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.count()
      e.fire(1)
      e.fire(1)
      assert(s.readValueOnce == 2)
    }

    /* iterate */
    test("iterate the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val f              = (x: Int) => x
      val s: Signal[Int] = e.iterate(10)(f)
      assert(s.readValueOnce == 10)
    }

    test("iterate the Function is Executed Every Time The Event Fires") {
      var test: Int      = 0
      val e              = Evt[Int]()
      val f              = (x: Int) => { test += 1; x }
      val s: Signal[Int] = e.iterate(10)(f)
      e.fire(1)
      assert(test == 1)
      e.fire(2)
      assert(test == 2)
      e.fire(1)
      assert(test == 3)
      assertEquals(s.readValueOnce, 10)
    }

    // TODO: does it make sense ?
    test("iterate the Parameter Is Always The Init Value") {
      var test: Int      = 0
      val e              = Evt[Int]()
      val f              = (x: Int) => { test = x; x + 1 }
      val s: Signal[Int] = e.iterate(10)(f)
      e.fire(1)
      assert(test == 10)
      e.fire(2)
      assert(test == 11)
      e.fire(1)
      assert(test == 12)
      assertEquals(s.readValueOnce, 13)
    }

    test("iterate the result signal does not depend on the event value") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.iterate(10)(identity)
      e.fire(1)
      assert(s.readValueOnce == 10)
      e.fire(2)
      assert(s.readValueOnce == 10)
      e.fire(1)
      assert(s.readValueOnce == 10)
    }

    /* latest */
    test("latest the Initial Value Is Set Correctly") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.hold(10)

      assert(s.readValueOnce == 10)
    }

    test("latest the Function is Executed Every Time The Event Fires") {
      val e              = Evt[Int]()
      val s: Signal[Int] = e.hold(10)

      e.fire(1)
      assert(s.readValueOnce == 1)
      e.fire(2)
      assert(s.readValueOnce == 2)
      e.fire(1)
      assert(s.readValueOnce == 1)
    }

    /* latestOption */
    test("latest Option the Initial Value Is Set Correctly") {
      val e                      = Evt[Int]()
      val s: Signal[Option[Int]] = e.holdOption()

      assert(s.readValueOnce == None)
    }

    test("latest Option the Function is Executed Every Time The Event Fires") {
      val e                      = Evt[Int]()
      val s: Signal[Option[Int]] = e.holdOption()

      e.fire(1)
      assert(s.readValueOnce == Option(1))
      e.fire(2)
      assert(s.readValueOnce == Option(2))
      e.fire(1)
      assert(s.readValueOnce == Option(1))
    }

    /* last */
    test("last the Initial Value Is Set Correctly") {
      val e                         = Evt[Int]()
      val s: Signal[LinearSeq[Int]] = e.list(5)

      assert(s.readValueOnce == List())
    }

    test("last collects The LastN Events") {
      val e                         = Evt[Int]()
      val s: Signal[LinearSeq[Int]] = e.list(5)

      assert(s.readValueOnce == LinearSeq())
      e.fire(1)
      assert(s.readValueOnce == LinearSeq(1))
      e.fire(2)
      assert(s.readValueOnce == LinearSeq(1, 2))

      e.fire(3)
      e.fire(4)
      e.fire(5)
      assert(s.readValueOnce == LinearSeq(1, 2, 3, 4, 5))
      e.fire(6)
      assert(s.readValueOnce == LinearSeq(2, 3, 4, 5, 6))
    }

    /* list */
    test("list the Initial Value Is Set Correctly") {
      val e = Evt[Int]()
      val s = e.list()

      assert(s.readValueOnce == List())
    }

    test("list the Function is Executed Every Time The Event Fires") {
      val e = Evt[Int]()
      val s = e.list()

      assert(s.readValueOnce == List())
      e.fire(1)
      assert(s.readValueOnce == List(1))
      e.fire(2)
      assert(s.readValueOnce == List(2, 1))

      e.fire(3)
      e.fire(4)
      e.fire(5)
      e.fire(6)
      assert(s.readValueOnce == List(6, 5, 4, 3, 2, 1))
    }

    test("create folds during tx") {

      val e = Evt[String]()
      val listed = transaction(e) { t ?=>
        e.admit("hello")
        e.list()
      }

      assert(listed.readValueOnce == List("hello"))

    }

    /* fold expressions */

    test("fold expression works") {

      val word  = Evt[String]()
      val count = Evt[Int]()
      val reset = Evt[Unit]()
      val res = Fold("")(
        reset branch (_ => ""),
        word branch identity,
        count branch (Fold.current * _)
      )

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
      transaction(count, word, reset) { at ?=>
        count.admit(2)
        word.admit("do them all!")
        reset.admit(())

      }
      assert(res.readValueOnce == "do them all!do them all!")
    }

    test("fold expression compiles with values of a subtype") {
      val e0 = Evt[Unit]()
      val e1 = Evt[Int]()
      val res = Fold(Option.empty[Int])(
        e0 branch { _ => Some(1) },
        e1 branch { _ => Option(2) }
      )

      assert(res.readValueOnce == None)
      e0.fire()
      assert(res.readValueOnce == Some(1))
      e1.fire(0)
      assert(res.readValueOnce == Some(2))
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
