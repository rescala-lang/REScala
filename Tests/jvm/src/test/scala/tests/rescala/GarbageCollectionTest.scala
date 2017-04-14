package tests.rescala

import java.lang.ref.{PhantomReference, ReferenceQueue}

import org.scalatest.prop.Whenever
import rescala.reactives.Observe
import rescala.testhelper.TestEngines

import scala.language.implicitConversions

class GarbageCollectionTest extends RETests with Whenever {


  allEngines("garbage collection for simple signal mappings"){engine => import engine._

    whenever(engine != TestEngines.stm) {

      val q = new ReferenceQueue[Signal[Array[Int]]]()

      def makeGarbage() = {
        val v1 = Var(0)
        val res = v1.map(_ => new Array[Int](1024 * 1024))
        val obs = res.observe(_ => Unit)
        obs.remove()
        Observe.weak(res)(_ => Unit, fail = null)

        val p = new PhantomReference(res, q)
        (v1, p)
      }

      var done = false
      val start = System.currentTimeMillis()

      var `heap of garbage` = List(makeGarbage())

      while (!done) {
        `heap of garbage` ::= makeGarbage()
        System.gc()
        val timeout = !(System.currentTimeMillis() < start + 10000)
        assert(!timeout, "did not GC a signal before timeout")
        if (q.poll() ne null) done = true
      }
    }

  }

}
