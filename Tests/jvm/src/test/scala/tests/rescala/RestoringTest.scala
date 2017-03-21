package tests.rescala

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import rescala.restore.{ReStore, ReStoringEngine}

class RestoringTest extends FunSuite with BeforeAndAfterEach {

  override def beforeEach(): Unit = {
    ReStore.values.clear()
    ReStore.crash()
    super.beforeEach() // To be stackable, must call super.beforeEach
  }

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


  test("save and restore with changes in between"){

    {
      implicit val engine = new ReStoringEngine()
      val e = engine.Evt[Unit]()
      val c = e.count()

      assert(c.now == 0)

      e.fire()
      e.fire()

      assert(c.now == 2)

      val mapped = c.map(_ + 10)

      assert(mapped.now == 12)

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

      val mapped = c.map(_ + 10)

      assert(mapped.now == 13)


    }
  }

}
