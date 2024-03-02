package tests.rescala.fullmv

import reactives.core.{ReInfo, ReSource}
import reactives.fullmv.{FullMVEngine, State}
import tests.rescala.testtools.{IgnoreOnGithubCiBecause, Spawn}

class PipeliningTest extends munit.FunSuite {
  if reactives.default.global.scheduler.isInstanceOf[FullMVEngine] then {
    import reactives.default.*

    implicit def assumeSignalsAreFullMV(sig: ReSource): ReSource.of[State] = sig.asInstanceOf

    val engine: FullMVEngine = reactives.default.global.scheduler.asInstanceOf[FullMVEngine]
    // IgnoreOnGithubCiBecause("pipelining does not work")
    test("pipelining works") {
      val millisecondsPerNode = 10L
      val pipelineLength      = 20
      val numberOfUpdates     = 10

      val input                       = Var(0)
      val derived: Array[Signal[Int]] = new Array(pipelineLength)
      for (i <- 0 until pipelineLength) {
        val from = if (i == 0) input else derived(i - 1)
        derived(i) = ReInfo.named("pipeline-" + i) { implicit ! =>
          from.map { v =>
            Thread.sleep(millisecondsPerNode)
            v + 1
          }
        }
      }
      var all = Seq.empty[Int]
      derived.last.observe(all :+= _)

      val leastPossibleMillisecondsWithoutPipelining = pipelineLength * numberOfUpdates * millisecondsPerNode

      val startTime = System.currentTimeMillis()
      val spawned   = for (_ <- 1 to numberOfUpdates) yield Spawn(input.transform(_ + 1))
      val timeout   = System.currentTimeMillis() + leastPossibleMillisecondsWithoutPipelining + 1000
      spawned.foreach(_.await(math.max(0, timeout - System.currentTimeMillis())))
      val endTime = System.currentTimeMillis()

      assertEquals(all, (pipelineLength to (pipelineLength + numberOfUpdates)))
      assert(endTime - startTime < leastPossibleMillisecondsWithoutPipelining)
    }
  }
}
