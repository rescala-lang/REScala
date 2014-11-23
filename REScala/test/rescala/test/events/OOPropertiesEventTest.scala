package rescala.test.events


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Implicits.default
import rescala.events._


/**
 * Demonstrates some of the features and of the limitations of the
 * current implementation w.r.t. OO design, like inheritance,
 * polymorphism, overriding, etc...
 */
class OOPropertiesEventTest extends AssertionsForJUnit with MockitoSugar {

  @Test def eventsAreInherited() = {

    var test = 0

    class A {
      val e1 = new ImperativeEvent[Int]()
      e1 += ((x: Int) => { test += 1 })
    }
    class B extends A {
      e1(10)
    }
    new B()
    assert(test == 1)
  }


  @Test def canTriggerEventsInSuperclass() = {

    var test = 0

    class A {
      val e1 = new ImperativeEvent[Int]()
    }
    class B extends A {
      e1 += ((x: Int) => { test += 1 })
      e1(10)
    }
    new B()
    assert(test == 1)
  }


  @Test def issueWithOverridingEvents(): Unit = {

    try {
      var test = 0

      class A {
        lazy val e1: Event[Int] = new ImperativeEvent[Int]()
        e1 += ((x: Int) => { test += 1 })
      }

      class B extends A {
        val e2 = new ImperativeEvent[Int]()
        val e3 = new ImperativeEvent[Int]()
        override lazy val e1: Event[Int] = e2 || e3
        e1 += ((x: Int) => { test += 1 })
        e2(10)
      }
      new B()

    }
    catch {
      case e: NullPointerException => return
    }
    assert(false)
  }


  class X {}
  class Y extends X {}

  @Test def refine() = {

    var test = 0

    class A {
      val e1: Event[X] = new ImperativeEvent[X]()
    }
    class B extends A {
      val e2 = new ImperativeEvent[Y]()
      val e3 = new ImperativeEvent[Y]()
      override val e1: Event[X] = e2 || e3
      e1 += ((x: X) => { test += 1 })
      e2(new Y)
    }
    new B()
    assert(test == 1)
  }

}
