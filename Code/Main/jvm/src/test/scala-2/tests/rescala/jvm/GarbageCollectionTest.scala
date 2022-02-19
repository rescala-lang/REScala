package tests.rescala.jvm

import java.lang.ref.{PhantomReference, ReferenceQueue}

import org.scalatest.prop.Whenever
import rescala.operator.Pulse
import tests.rescala.testtools.RETests

class GarbageCollectionTest extends RETests with Whenever {
  multiEngined { engine =>
    import engine._

    "garbage collection for simple signal mappings" in {

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

      while (!done) {
        `heap of garbage` ::= makeGarbage()
        engine.transaction(`heap of garbage`.map(_._1): _*) { at =>
          `heap of garbage`.iterator.map(_._1).foreach(_.admitPulse(Pulse.Value(1))(at))
        }
        System.gc()
        val timeout = !(System.currentTimeMillis() < start + 100000)
        assert(!timeout, "did not GC a signal before timeout")
        if (!(q.poll() eq null)) done = true
      }
    }

  }
}
