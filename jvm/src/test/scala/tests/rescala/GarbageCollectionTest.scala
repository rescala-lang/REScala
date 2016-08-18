package tests.rescala

import java.lang.ref.{PhantomReference, ReferenceQueue}

import org.scalatest.prop.Whenever
import rescala.engines.Engines

import scala.language.implicitConversions

class GarbageCollectionTest extends RETests with Whenever {


  allEngines("garbage collection for simple signal mappings"){engine => import engine._

    whenever(engine != Engines.stm) {

      val q = new ReferenceQueue[Signal[Int]]()

      def makeGarbage() = {
        val v1 = Var(0)
        val res = v1.map(identity)
        val p = new PhantomReference(res, q)
        (v1, p)
      }

      val `heap of garbage` = 1 to 1000 map (_ => makeGarbage())

      var done = false
      val start = System.currentTimeMillis()

      while (!done) {
        System.gc()
        val timeout = !(System.currentTimeMillis() < start + 1000)
        assert(!timeout, "did not GC a signal before timeout")
        if (q.poll() ne null) done = true
      }
    }

  }

}
