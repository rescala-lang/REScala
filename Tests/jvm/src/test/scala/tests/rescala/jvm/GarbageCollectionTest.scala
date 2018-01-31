package tests.rescala.jvm

import java.lang.ref.{PhantomReference, ReferenceQueue}

import org.scalatest.prop.Whenever
import rescala.reactives.Observe
import rescala.stm.STMEngine
import tests.rescala.testtools.RETests

class GarbageCollectionTest extends RETests with Whenever { multiEngined { engine => import engine._

  if(engine == STMEngine.stm) { test("nothing"){} }
else {

  test("garbage collection for simple signal mappings"){


      val q = new ReferenceQueue[Signal[Array[Int]]]()

      def makeGarbage() = {
        val v1 = Var(0)
        val res = v1.map(_ => new Array[Int](1024 * 1024))
        val obs = res.observe(_ => Unit)
        obs.remove()
        Observe.weak(res, fireImmediately = true)((_: Array[Int]) => Unit, fail = null)

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

}}
