package tests.rescala.static.conversions

import reactives.structure.RExceptions.EmptySignalControlThrowable
import reactives.structure.Pulse
import tests.rescala.testtools.RETests

import reactives.default.*

class Change extends RETests {

    /* changed */
    test("changed is Not Triggered On Creation") {
      var test          = 0
      val v1            = Var(1)
      val s1            = v1.map { _ + 1 }
      val e: Event[Int] = s1.changed
      e observe ((_: Int) => { test += 1 })

      assert(test == 0)
    }

    test("changed is Triggered When The Signal Changes") {
      var test          = 0
      val v1            = Var(1)
      val s1            = v1.map { _ + 1 }
      val e: Event[Int] = s1.changed
      e observe ((_: Int) => { test += 1 })

      v1 set 2
      assert(test == 1)
      v1 set 3
      assert(test == 2)
    }

    test("changed the Value Of The Event Reflects The Change In The Signal") {
      var test          = 0
      val v1            = Var(1)
      val s1            = v1.map { _ + 1 }
      val e: Event[Int] = s1.changed
      e observe ((x: Int) => { test = x })

      v1 set 2
      assert(test == 3)
      v1 set 3
      assert(test == 4)
    }

    /* changedTo */
    test("changed To is Not Triggered On Creation") {
      var test          = 0
      val v1            = Var(1)
      val s1            = v1.map { _ + 1 }
      val e: Event[Any] = s1.changed.filter(_ == 1)
      e observe ((_: Any) => { test += 1 })

      assert(test == 0)
    }

    test("changed To is Triggered When The Signal Has The Given Value") {
      var test = 0
      val v1   = Var(1)
      val s1   = v1.map { _ + 1 }
      val e    = s1.changed.filter(_ == 3)
      e observe ((_) => { test += 1 })

      v1 set 2
      assert(test == 1)
      v1 set 3
      assert(test == 1)
    }

    /* change */
    test("change is Not Triggered On Creation") {
      var test = 0
      val v1   = Var(1)
      val s1   = v1.map { _ + 1 }
      val e    = s1.change
      e observe { _ => test += 1 }

      assert(test == 0)
    }

    test("change is Triggered When The Signal Changes") {
      var test = 0
      val v1   = Var(1)
      val s1   = v1.map { _ + 1 }
      val e    = s1.change
      e observe { _ => test += 1 }

      assertEquals(test, 0)
      assertEquals(s1.readValueOnce, 2)
      v1 set 2
      assertEquals(s1.readValueOnce, 3)
      assertEquals(test, 1)
      v1 set 3
      assertEquals(s1.readValueOnce, 4)
      assertEquals(test, 2)
    }

    test("change the Value Of The Event Reflects The Change In The Signal") {
      var test = (0, 0)
      val v1   = Var(1)
      val s1   = v1.map { _ + 1 }
      val e    = s1.change
      e observe { x => test = x.pair }

      v1 set 2
      assertEquals(test, ((2, 3)))
      v1 set 3
      assertEquals(test, ((3, 4)))
    }

    /* with empty signals */

    test("changing emptiness") {
      val v2 = Var.empty[String]

      val e2 = v2.change.map(_.pair).recover { case t => Some("failed" -> t.toString) }

      val ored: Event[(String, String)] = e2

      val log = ored.list()

      assertEquals(log.readValueOnce, Nil)

      v2.set("two")
      assertEquals(log.readValueOnce, List())

      v2.set("three")
      assertEquals(log.readValueOnce, List("two" -> "three"))
    }

    test("folding changing and emptiness") {
      val v1 = Var.empty[String]
      val v2 = Var.empty[String]

      val e1 = v1.changed.map(x => ("constant", x))
      val e2 = v2.change.map(_.pair).recover { case t => Some("failed" -> t.toString) }

      val ored: Event[(String, String)] = e1 || e2

      val log = ored.list()

      assertEquals(log.readValueOnce, Nil)

      v1.set("one")
      assertEquals(log.readValueOnce, List("constant" -> "one"))

      v2.set("two")
      assertEquals(log.readValueOnce, List("constant" -> "one"))

      v2.set("three")
      assertEquals(log.readValueOnce, List("two" -> "three", "constant" -> "one"))

      transaction(v1, v2) { at =>
        v1.admit("four a")(at)
        v2.admit("four b")(at)
      }

      assertEquals(log.readValueOnce, List("constant" -> "four a", "two" -> "three", "constant" -> "one"))

      transaction(v1, v2) { turn =>
        v1.admitPulse(Pulse.Exceptional(EmptySignalControlThrowable(summon)))(turn)
        v2.admit("five b")(turn)
      }

      assertEquals(log.readValueOnce, List(
        "four b"   -> "five b",
        "constant" -> "four a",
        "two"      -> "three",
        "constant" -> "one"
      ))

    }

}
