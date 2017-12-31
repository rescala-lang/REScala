package tests.rescala.events

import tests.rescala.RETests


/**
 * Demonstrates some of the features and of the limitations of the
 * current implementation w.r.t. OO design, like inheritance,
 * polymorphism, overriding, etc...
 */

class OOPropertiesEventTest extends RETests {




  allEngines("events Are Inherited"){ engine => import engine._

    var test = 0

    class A {
      val e1 = Evt[Int]
      e1 += ((x: Int) => { test += 1 })
    }
    class B extends A {
      e1.fire(10)
    }
    new B()
    assert(test == 1)
  }


  allEngines("can Trigger Events In Superclass"){ engine => import engine._

    var test = 0

    class A {
      val e1 = Evt[Int]
    }
    class B extends A {
      e1 += ((x: Int) => { test += 1 })
      e1.fire(10)
    }
    new B()
    assert(test == 1)
  }


  allEngines("issue With Overriding Events"){ engine => import engine._

     intercept[Exception] {
      var test = 0

      class A {
        lazy val e1: Event[Int] = Evt[Int]
        // this will force e1 which is overriden below
        e1 += ((x: Int) => { test += 1 })
      }

      class B extends A {
        val e2 = Evt[Int]
        val e3 = Evt[Int]
        // but this override here requires e2 and e3 which are not yet initialized
        override lazy val e1: Event[Int] = e2 || e3
        e1 += ((x: Int) => { test += 1 })
        e2.fire(10)
      }
      new B()

    }
  }


  class X {}
  class Y extends X {}

  allEngines("refine"){ engine => import engine._

    var test = 0

    class A {
      val e1: Event[X] = Evt[X]
    }
    class B extends A {
      val e2 = Evt[Y]
      val e3 = Evt[Y]
      override val e1: Event[X] = e2 || e3
      e1 += ((x: X) => { test += 1 })
      e2.fire(new Y)
    }
    new B()
    assert(test == 1)
  }

}
