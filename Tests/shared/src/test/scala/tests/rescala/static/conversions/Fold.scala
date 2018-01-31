package tests.rescala.static.conversions

import tests.rescala.testtools.RETests

import scala.collection.LinearSeq


class Fold extends RETests { multiEngined { engine => import engine._

  /* fold */
  test("fold the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    assert(s.now == 10)
  }

  test("fold the Result Signal Increases When Events Occur") {
    val e = Evt[Int]
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    e.fire(1)
    e.fire(1)
    assert(s.now == 12)
  }


  /* count */
  test("count the Initial Value Is Set Correctly"){
    val e = Evt[Int]
    val s: Signal[Int] = e.count
    assert(s.now == 0)
  }

  test("count the Result Signal Increases When Events Occur"){
    val e = Evt[Int]
    val s: Signal[Int] = e.count
    e.fire(1)
    e.fire(1)
    assert(s.now == 2)
  }


  /* iterate */
  test("iterate the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val f = (x: Int) => x
    val s: Signal[Int] = e.iterate(10)(f)
    assert(s.now == 10)
  }

  test("iterate the Function is Executed Every Time The Event Fires") {
    var test: Int = 0
    val e = Evt[Int]
    val f = (x: Int) => {test += 1; x}
    val s: Signal[Int] = e.iterate(10)(f)
    e.fire(1)
    assert(test == 1)
    e.fire(2)
    assert(test == 2)
    e.fire(1)
    assert(test == 3)
    assert(s.now === 10)
  }

  // TODO: does it make sense ?
  test("iterate the Parameter Is Always The Init Value") {
    var test: Int = 0
    val e = Evt[Int]
    val f = (x: Int) => {test = x; x + 1}
    val s: Signal[Int] = e.iterate(10)(f)
    e.fire(1)
    assert(test == 10)
    e.fire(2)
    assert(test == 11)
    e.fire(1)
    assert(test == 12)
    assert(s.now === 13)
  }

  test("iterate the result signal does not depend on the event value") {
    val e = Evt[Int]
    val s: Signal[Int] = e.iterate(10)(identity)
    e.fire(1)
    assert(s.now == 10)
    e.fire(2)
    assert(s.now == 10)
    e.fire(1)
    assert(s.now == 10)
  }

  /* latest */
  test("latest the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val s: Signal[Int] = e.latest(10)

    assert(s.now == 10)
  }

  test("latest the Functionis Executed Every Time The Event Fires") {
    val e = Evt[Int]
    val s: Signal[Int] = e.latest(10)

    e.fire(1)
    assert(s.now == 1)
    e.fire(2)
    assert(s.now == 2)
    e.fire(1)
    assert(s.now == 1)
  }


  /* latestOption */
  test("latest Option the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val s: Signal[Option[Int]] = e.latestOption()

    assert(s.now == None)
  }

  test("latest Option the Functionis Executed Every Time The Event Fires") {
    val e = Evt[Int]
    val s: Signal[Option[Int]] = e.latestOption()

    e.fire(1)
    assert(s.now == Option(1))
    e.fire(2)
    assert(s.now == Option(2))
    e.fire(1)
    assert(s.now == Option(1))
  }


  /* last */
  test("last the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.now == List())
  }

  test("last collects The LastN Events") {
    val e = Evt[Int]
    val s: Signal[LinearSeq[Int]] = e.last(5)


    assert(s.now == LinearSeq())
    e.fire(1)
    assert(s.now == LinearSeq(1))
    e.fire(2)
    assert(s.now == LinearSeq(1, 2))

    e.fire(3)
    e.fire(4)
    e.fire(5)
    assert(s.now == LinearSeq(1, 2, 3, 4, 5))
    e.fire(6)
    assert(s.now == LinearSeq(2, 3, 4, 5, 6))
  }

  /* list */
  test("list the Initial Value Is Set Correctly") {
    val e = Evt[Int]
    val s = e.list()

    assert(s.now == List())
  }

  test("list the Functionis Executed Every Time The Event Fires") {
    val e = Evt[Int]
    val s = e.list()

    assert(s.now == List())
    e.fire(1)
    assert(s.now == List(1))
    e.fire(2)
    assert(s.now == List(2, 1))

    e.fire(3)
    e.fire(4)
    e.fire(5)
    e.fire(6)
    assert(s.now == List(6, 5, 4, 3, 2, 1))
  }

  test("create folds during tx"){

    val e = Evt[String]

    val listed = transaction(e) { implicit t =>
      e.admit("hello")
      e.list()
    }

    assert(listed.now == List("hello"))

  }


  /* fold expressions */

  test("fold expression works"){

    val word = Evt[String]
    val count = Evt[Int]
    val reset = Evt[Unit]

    val res = Events.foldAll(""){ acc => Events.Match(
      reset >> (_ => ""),
      word >> identity,
      count >> (acc * _),
    )}

    assert (res.now == "")
    count.fire(10)
    assert (res.now == "")
    reset.fire()
    assert (res.now == "")
    word.fire("hello")
    assert (res.now == "hello")
    count.fire(2)
    assert (res.now == "hellohello")
    word.fire("world")
    assert (res.now == "world")
    update(count -> 2, word -> "do them all!", reset -> (()))
    assert (res.now == "do them all!do them all!")
  }

} }
