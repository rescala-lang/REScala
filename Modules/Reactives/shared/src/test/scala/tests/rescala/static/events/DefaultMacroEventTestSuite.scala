package tests.rescala.static.signals

import org.scalatest.freespec.AnyFreeSpec
import rescala.default._

class DefaultMacroEventTestSuite extends AnyFreeSpec {
  "default event expression works" in {
    val ev1 = Evt[Int]()
    val ev2 = Event { ev1() }
    val ev3 = ev2 collect { case e if e < 10 => -e }

    val res = ev3.latest(0)

    assert(res.readValueOnce === 0)
    ev1.fire(9)
    assert(res.readValueOnce === -9)
    ev1.fire(13)
    assert(res.readValueOnce === -9)
  }
}
