package tests.rescala.jvm

import reactives.default as engine
import reactives.default.*
import reactives.structure.Pulse
import tests.rescala.testtools.RETests

import java.lang.ref.{PhantomReference, ReferenceQueue}

class GarbageCollectionTest extends RETests {

  test("garbage collection for simple signal mappings") {

    val q = new ReferenceQueue[Signal[Array[Int]]]()

    def makeGarbage() = {
      val v1  = Var(0)
      val res = v1.map(_ => new Array[Int](1024 * 1024))
      val obs = res.observe(_ => ())
      obs.disconnect()
      res.disconnect()
      val p = new PhantomReference(res, q)
      (v1, p)
    }

    var done  = false
    val start = System.currentTimeMillis()

    var `heap of garbage` = List(makeGarbage())

    while !done do {
      `heap of garbage` ::= makeGarbage()
      engine.transaction(`heap of garbage`.map(_._1)*) { at ?=>
        `heap of garbage`.iterator.map(_._1).foreach(_.admitPulse(Pulse.Value(1))(using at))
      }
      System.gc()
      val timeout = !(System.currentTimeMillis() < start + 100000)
      assert(!timeout, "did not GC a signal before timeout")
      if !(q.poll() eq null) then done = true
    }
  }

}
