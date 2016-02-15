package rescala.pipelining.tests

import java.util.concurrent.Semaphore

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.PipelineEngine
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.reactives.{Signals, Var}

class LateDynamicReadTest extends AssertionsForJUnit with MockitoSugar {

  implicit val engine = new PipelineEngine()


  val source1 = Var(0)
  val depWait = Signals.lift(source1) ( _ + 1)
  val depTakeLong = Signals.lift(depWait) (x => { Thread.sleep(1000); x + 1})

  val source2 = Var(0)
  val depConnect1And2 = Signals.lift(source1, source2) {(x,y) => x + y}

  val source3 = Var(0)
  val delayDynamic = Signals.lift(source3) (x => {Thread.sleep(500); x})
  val dynamic = Signals.dynamic(source3)(turn  => if (source3(turn) %2 == 0) 0 else source2(turn))
  val depDynamic = Signals.lift(dynamic)(_ + 1)

  @Test
  def testSequential() = {
    source1.set(1)
    source3.set(1)
    source2.set(1)
    assert(depTakeLong.now == 3)
    assert(depConnect1And2.now == 2)
    assert(dynamic.now == 1)
    assert(depDynamic.now == 2)
  }

  @Test
  def testParallelDynamicAddIsPropagated() = {
    val initTurnStarted = new Semaphore(0)
    val newDynamicStarted = new Semaphore(0)

    val initTurnWhichKeepsAllFollowingTurnsAlive = createThread{
      engine.plan(source1) { implicit turn =>
        initTurnStarted.release()
        source1.admit(1)
      }
    }

    val addDynamicDependencyTurn = createThread {
      initTurnStarted.acquire()
      engine.plan(source3) {implicit turn =>
        newDynamicStarted.release()
        source3.admit(1)
      }
    }

    val updateNewDynamicDependency = createThread {
      newDynamicStarted.acquire()
      source2.set(1)
    }

    initTurnWhichKeepsAllFollowingTurnsAlive.start()
    addDynamicDependencyTurn.start()
    updateNewDynamicDependency.start()

    initTurnWhichKeepsAllFollowingTurnsAlive.join()
    addDynamicDependencyTurn.join()
    updateNewDynamicDependency.join()

    assert(depTakeLong.now == 3)
    assert(depConnect1And2.now == 2)
    assert(dynamic.now == 1)
    assert(depDynamic.now == 2)

  }

}