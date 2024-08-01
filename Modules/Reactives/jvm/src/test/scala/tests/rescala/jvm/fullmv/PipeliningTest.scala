package tests.rescala.fullmv

import reactives.core.{ReInfo, ReSource}
import reactives.fullmv.{FullMVEngine, State}
import tests.rescala.testtools.Spawn

import scala.language.implicitConversions

class PipeliningTest extends munit.FunSuite {
  if reactives.SelectedScheduler.candidate.scheduler.isInstanceOf[FullMVEngine] then {
    import reactives.default.*

    // given assumeSignalsAreFullMV(sig: ReSource): ReSource.of[State] = sig.asInstanceOf

    val engine: FullMVEngine = reactives.SelectedScheduler.candidate.scheduler.asInstanceOf[FullMVEngine]
    test("pipelining works") {
      val millisecondsPerNode = 10L
      val pipelineLength      = 20
      val numberOfUpdates     = 10

      val input                       = Var(0)
      val derived: Array[Signal[Int]] = new Array(pipelineLength)
      for i <- 0 until pipelineLength do {
        val from = if i == 0 then input else derived(i - 1)
        derived(i) = ReInfo.named("pipeline-" + i) {
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
      val spawned   = for _ <- 1 to numberOfUpdates yield Spawn(input.transform(_ + 1))
      val timeout   = System.currentTimeMillis() + leastPossibleMillisecondsWithoutPipelining + 1000
      spawned.foreach(_.await(math.max(0, timeout - System.currentTimeMillis())))
      val endTime = System.currentTimeMillis()

      assertEquals(all, (pipelineLength to (pipelineLength + numberOfUpdates)))
      assert(endTime - startTime < leastPossibleMillisecondsWithoutPipelining)
    }
  }
}
