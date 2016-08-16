package tests.rescala


import java.lang.ref.{ReferenceQueue, PhantomReference}

import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Spores
import rescala.turns.{Engine, Turn}


import scala.language.implicitConversions

object GarbageCollectionTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class GarbageCollectionTest[S <: Spores](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Evt, Var, Signal, Event}


  @Ignore def `garbage collection for simple signal mappings`() = {

    val q = new ReferenceQueue[Var[Int]]()

    def makeGarbage() = {
      val v1 = Var(0)
      val res = v1.map(identity).map(identity)
      val p = new PhantomReference(v1, q)
      (res, p)
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
