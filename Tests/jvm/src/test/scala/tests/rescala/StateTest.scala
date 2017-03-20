package tests.rescala

import org.scalatest.FunSuite
import rescala.restore.{ReStore, ReStoringEngine}

class StateTest extends FunSuite {

  test("simple save and restore"){

    {
      implicit val engine = new ReStoringEngine()
      val e = engine.Evt[Unit]()
      val c = e.count()

      assert(c.now == 0)

      e.fire()
      e.fire()

      assert(c.now == 2)
    }

    ReStore.crash()
    println(ReStore.values)

    {
      implicit val engine1 = new ReStoringEngine()
      val e = engine1.Evt[Unit]()
      val c = e.count()

      assert(c.now == 2)

      e.fire()

      assert(c.now == 3)
    }
  }

}
