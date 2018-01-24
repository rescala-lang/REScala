package tests.rescala.static.conversions

import tests.rescala.util.RETests

import scala.collection.LinearSeq


class Fold extends RETests {

  /* fold */
  allEngines("fold the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    assert(s.now == 10)
  }

  allEngines("fold the Result Signal Increases When Events Occur") { engine => import engine._
    val e = Evt[Int]
    val f = (x: Int, y: Int) => x + y
    val s: Signal[Int] = e.fold(10)(f)
    e.fire(1)
    e.fire(1)
    assert(s.now == 12)
  }


  /* count */
  allEngines("count the Initial Value Is Set Correctly"){ engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.count
    assert(s.now == 0)
  }

  allEngines("count the Result Signal Increases When Events Occur"){ engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.count
    e.fire(1)
    e.fire(1)
    assert(s.now == 2)
  }


  /* iterate */
  allEngines("iterate the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val f = (x: Int) => x
    val s: Signal[Int] = e.iterate(10)(f)
    assert(s.now == 10)
  }

  allEngines("iterate the Functionis Executed Every Time The Event Fires") { engine => import engine._
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
  allEngines("iterate the Parameter Is Always The Init Value") { engine => import engine._
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

  allEngines("iterate the result signal does not depend on the event value") { engine => import engine._
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
  allEngines("latest the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[Int] = e.latest(10)

    assert(s.now == 10)
  }

  allEngines("latest the Functionis Executed Every Time The Event Fires") { engine => import engine._
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
  allEngines("latest Option the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[Option[Int]] = e.latestOption()

    assert(s.now == None)
  }

  allEngines("latest Option the Functionis Executed Every Time The Event Fires") { engine => import engine._
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
  allEngines("last the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val s: Signal[LinearSeq[Int]] = e.last(5)

    assert(s.now == List())
  }

  allEngines("last collects The LastN Events") { engine => import engine._
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
  allEngines("list the Initial Value Is Set Correctly") { engine => import engine._
    val e = Evt[Int]
    val s = e.list()

    assert(s.now == List())
  }

  allEngines("list the Functionis Executed Every Time The Event Fires") { engine => import engine._
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

  allEngines("create folds during tx"){ engine => import engine._

    val e = Evt[String]

    val listed = transaction(e) { implicit t =>
      e.admit("hello")
      e.list()
    }

    assert(listed.now == List("hello"))

  }

}
