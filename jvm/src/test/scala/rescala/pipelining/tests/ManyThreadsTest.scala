package rescala.pipelining.tests

import java.util.Random
import java.util.concurrent.CyclicBarrier

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.util.LogUtils
import rescala.reactives.Var
import rescala.pipelining.Pipeline._
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.pipelining.{PipelineEngine, PipelineStruct}
import rescala.propagation.Turn
import rescala.reactives.Signals

import scala.annotation.tailrec

class ManyThreadsTest extends AssertionsForJUnit  {

  implicit val engine = new PipelineEngine()
  type S = PipelineStruct.type

  /*
   * This test suite runs on the following topology: S1 and S2 are sources
   * and D1 and D2 are dependencies
   *
   * S1    S2
   * | \  / |
   * |  \/  |
   * |  /\  |
   * | /  \ |
   * vv    vv
   * D1    D2
   */

  var opsOnD1: List[Turn[S]] = List()
  var opsOnD2: List[Turn[S]] = List()

  def clearOps(): Unit = {
    opsOnD1 = List()
    opsOnD2 = List()
  }

  var calculatesOn1 = false
  var calculatesOn2 = false

  var enableCheck = false;
  def checkCalculationOrder(): Unit = {
    @tailrec
    def check(ops1: List[Turn[S]], ops2: List[Turn[S]]): Boolean = {
      if (ops1.isEmpty || ops2.isEmpty)
        true
      else if (ops1.head != ops2.head)
        false
      else check(ops1.tail, ops2.tail)
    }
    if (enableCheck) {
      val op1 = opsOnD1
      val op2 = opsOnD2
      val ok = check(op1, op2)
      assyncAssert(ok, s"$op1 $op2")
    }
  }

  def assyncAssert(ok: => Boolean, message : =>String = ""): Unit = if (!ok) throw new AssertionError(s"$message\n")


  val s1 = Var(0)
  val s2 = Var(0)
  val d1 = Signals.static(s1, s2) { implicit t =>
    randomWait {
      assyncAssert(!calculatesOn1)
      calculatesOn1 = true
      opsOnD1 :+= t
      checkCalculationOrder
      val newVal = s1.get - s2.get
      calculatesOn1 = false
      newVal
    }
  }
  val d2 = Signals.static(s1, s2) { implicit t =>
    randomWait {
      assert(!calculatesOn2)
      calculatesOn2 = true
      opsOnD2 :+= t
      checkCalculationOrder
      val newVal = s1.get - 2 * s2.get
      calculatesOn2 = false
      newVal
    }
  }

  @Test (timeout = 30000)
  def testEvaluationParallel(): Unit = {

    for (i <- 1 to 100) {
      LogUtils.log("------")
      val update1 = createThread {
        s1.set(10)
      }
      val update2 = createThread {
        s2.set(5)
      }
      clearOps

      if (i % 2 == 0) {
        update1.start
        update2.start
      } else {
        update2.start
        update1.start
      }

      update1.join
      update2.join

      assert(pipelineFor(s1).getPipelineFrames().isEmpty)
      assert(pipelineFor(s2).getPipelineFrames().isEmpty)
      assert(pipelineFor(d1).getPipelineFrames().isEmpty)
      assert(pipelineFor(d2).getPipelineFrames().isEmpty)

      //assert(engine.getOrdering.isEmpty)
      // assert(engine.getWaitingEdges.isEmpty)

      // Order of both turns needs to be equal at both dependencies
      assert(opsOnD1 == opsOnD2)

      // Now either update1 was scheduled first or update2
      // Independent of the if statement above

      assert(d1.now == 5)
      assert(d2.now == 0)

      clearOps

      s1.set(0)
      s2.set(0)

    }
  }

  @Test (timeout = 10000)
  def testManyThreads(): Unit = {

    LogUtils.log("------")

    val rand = new Random
    val numThreads = 20
    val barrier = new CyclicBarrier(numThreads)
    val updateValues = List.iterate(1, numThreads)(x =>x+1)
    val updateSources = List.iterate(s1, numThreads)(x => if (x == s1) s2 else s1)
    val updateThreads = updateSources.zip(updateValues).map(
        { case (source, value) =>
          createThread {
            barrier.await()
            source.set(value) } })

    assert(updateThreads.size == numThreads)


    clearOps

    enableCheck = true
    updateThreads.foreach { _.start }
    updateThreads.foreach { _.join }

    assert(engine.getTurnOrder().isEmpty)
    assert(pipelineFor(s1).getPipelineFrames().isEmpty)
    assert(pipelineFor(s2).getPipelineFrames().isEmpty)
    assert(pipelineFor(d1).getPipelineFrames().isEmpty)
    assert(pipelineFor(d2).getPipelineFrames().isEmpty)

    //  assert(engine.getOrdering.isEmpty)
    //  assert(engine.getWaitingEdges.isEmpty)

    // Order of both turns needs to be equal at both dependencies
    assert(opsOnD1.length == numThreads )
    assert(opsOnD2.length == numThreads )
    assert(opsOnD1 == opsOnD2)

    clearOps


  }

}
