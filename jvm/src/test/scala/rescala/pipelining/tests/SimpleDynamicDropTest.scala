package rescala.pipelining.tests

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Ticket
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.pipelining.util.LogUtils
import rescala.pipelining.{Pipeline, PipelineEngine}

class SimpleDynamicDropTest extends AssertionsForJUnit  {

  implicit val engine = new PipelineEngine()

  import engine.Var

  val timeToAllowOtherTurnToCreateFrames = 100l

  val source1 = Var(0)
  val source2 = Var(2)
  var numEvaluated = 0
  val dynDep = engine.dynamic()(implicit t => {
    LogUtils.log(s"BEGIN evaluate $t")
    numEvaluated += 1
    Thread.sleep(timeToAllowOtherTurnToCreateFrames)
    val source1Val = source1(t)
    LogUtils.log(s"Read val $source1Val")
    val newValue = if (source1Val % 2 == 0) {
      source2(t)
    }
    else {
      0
    }
    LogUtils.log(s"END evaluate $t with $newValue")
    newValue
  })(Ticket(Right(engine)))

  val dynDepTracker = new ValueTracker(dynDep)

  dynDepTracker.reset()
  numEvaluated = 0

  @Test
  def serialDropTest(): Unit = {
    LogUtils.log("=====")
    assert(dynDep.now == 2)
    source1.set(1)
    assert(dynDep.now == 0)
    assert(numEvaluated == 1)
    numEvaluated = 0
    PipelineTestUtils.readLatestValue { implicit turn =>
      assert(source2.bud.outgoing == Set())
      assert(source1.bud.outgoing == Set(dynDep))
      assert(dynDep.bud.incoming == Set(source1))
    }
    source2.set(93)
    assert(dynDep.now == 0)
    assert(numEvaluated == 0)
    assert(dynDepTracker.values == List(0))
  }

  @Test
  def parallelAddAndRemove(): Unit = {
    var removeBeforeAdd = false
    var addBeforeRemove = false

    // Hope that thread scheduling is not deterministic enough to cover both cases with not too many tries
    while (!removeBeforeAdd || !addBeforeRemove) {

      source1.set(0)
      source2.set(2)

      PipelineTestUtils.readLatestValue { implicit turn =>
        assert(source1.bud.outgoing == Set(dynDep))
        assert(source2.bud.outgoing == Set(dynDep))
        assert(dynDep.bud.incoming == Set(source1, source2))
      }

      numEvaluated = 0
      dynDepTracker.reset()

      LogUtils.log("")
      LogUtils.log("")

      val threadRemoveDep = createThread(source1.set(1))
      val threadAddDep = createThread(source1.set(2))

      if (addBeforeRemove) {
        threadRemoveDep.start()
        threadAddDep.start()
      }
      else {
        threadAddDep.start()
        threadRemoveDep.start()
      }

      threadRemoveDep.join()
      threadAddDep.join()

      PipelineTestUtils.readLatestValue { implicit turn =>
        dynDep.now match {
          case 0 =>
            LogUtils.log("==> Add before remove")
            assert(dynDep.bud.incoming == Set(source1))
            assert(dynDep.now == 0)
            assert(source1.bud.outgoing == Set(dynDep))
            assert(source2.bud.outgoing == Set())
            assert(numEvaluated == 2)
            assert(dynDepTracker.values == List(0)) // Only one change because the change 2 -> 2 is not observed
            addBeforeRemove = true
          case 2 =>
            LogUtils.log("==> Remove before add")
            assert(dynDep.now == 2)
            assert(dynDep.bud.incoming == Set(source1, source2))
            assert(source1.bud.outgoing == Set(dynDep))
            assert(source2.bud.outgoing == Set(dynDep))
            removeBeforeAdd = true
            assert(numEvaluated == 2)
            assert(dynDepTracker.values == List(0, 2))
          case _ => fail("Invalid result value")
        }
      }

    }
  }

  @Test //(timeout = 10000)
  def parallelRemoveAndUpdateFromRemovedDep(): Unit = {
    var removeBeforeUpdateSuspicious = false
    var updateBeforeRemove = false

    // Again trust the nondeterminicness of execution
    while (!removeBeforeUpdateSuspicious || !updateBeforeRemove) {

      LogUtils.log("")
      LogUtils.log("")

      val removeDepThread = createThread(source1.set(1))
      val updateDepThread = createThread(source2.set(100))

      numEvaluated = 0
      dynDepTracker.reset()

      // Lets the the scheduler are bit to decide which thread starts creating frames
      if (updateBeforeRemove) {
        removeDepThread.start()
        updateDepThread.start()
      }
      else {
        updateDepThread.start()
        removeDepThread.start()
      }

      removeDepThread.join()
      updateDepThread.join()

      implicit val dummyTurn = engine.makeTurn
      engine.addTurn(dummyTurn)

      assert(Pipeline.pipelineFor(dynDep).getPipelineFrames().isEmpty)

      // in scheduling case, there should no dependency to source2
      assert(dynDep.bud.incoming == Set(source1))
      assert(source2.bud.outgoing == Set())
      assert(source1.bud.outgoing == Set(dynDep))

      engine.turnCompleted(dummyTurn)

      // But scheduling defines seen values
      numEvaluated match {
        case 1 =>

          //    case 0 =>
          //     LogUtils.log("==> Remove before Update no suspicious frame")
          // I dont force to get this result because it is equal to sequential execution
          // and hard to get scheduled in parallel
          //   case 1 =>
          LogUtils.log("==> Remove before Update suspicious frame")
          removeBeforeUpdateSuspicious = true
          //   case x =>
          //    fail(s"Illegal number of suspicious and not evaluated frames $x")
          // }
          assert(dynDepTracker.values == List(0))
        case 2 =>
          LogUtils.log("==> Update before remove")
          updateBeforeRemove = true
          assert(dynDepTracker.values == List(100, 0))
        case _ =>
          fail("Invalid number of evaluations")
      }

      // Reset graph
      source1.set(0)
      source2.set(2)
    }

  }

}
