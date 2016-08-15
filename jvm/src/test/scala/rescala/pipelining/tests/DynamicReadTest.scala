package rescala.pipelining.tests

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.PipelineEngine
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.pipelining.util.LogUtils
import rescala.reactives.{Signals, Var}

/**
 * @author moritzlichter
 */
class DynamicReadTest extends AssertionsForJUnit  {

  implicit val engine = new PipelineEngine()

  val minEvaluationTimeOfUpdate = 500l
  val letOtherUpdateCreateFramesTime = 200l

  assert(letOtherUpdateCreateFramesTime < minEvaluationTimeOfUpdate) // Such that the frames are still there
  LogUtils.log("")
  LogUtils.log("")

  val source1 = Var(0)
  val source2 = Var(100)

  val dynamicDep = engine.dynamic()(implicit t => {
    LogUtils.log(s"$t: Evaluate base ${source1(t)}")
    if (source1(t) % 2 != 0)
      source2(t)
    else 0
  })
  val depOfDynamic = Signals.static(dynamicDep)(implicit t => {
    Thread.sleep(minEvaluationTimeOfUpdate)
    LogUtils.log(s"$t: Eval of dyn dep completed")
    dynamicDep.get + 1
  })

  val source2Dep = Signals.static(source2)(implicit t => {
    Thread.sleep(minEvaluationTimeOfUpdate)
    LogUtils.log(s"$t: Eval of source2 dep completed")
    source2.get + 1
  })

  /*
   * Initial dependency graph
   *
   * source2          source1
   *   |                |
   *   |                v
   *   |           dynamicDep
   *   |                |
   *   v                v
   * source2Dep    depOfDynamic
   *
   *
   */

  /*
   * If the value of source1 is odd, we have
   *
   * source2          source1
   *   |    \           |
   *   |     \          v
   *   |      \---> dynamicDep
   *   |                |
   *   v                v
   * source2Dep    depOfDynamic
   *
   */

  // Track the value
  val depOfDynamicTracker = new ValueTracker(depOfDynamic)
  val source2DepTracker = new ValueTracker(source2Dep)

  @Test
  def addDynamicDependency1Before2(): Unit = {
    LogUtils.log("======")
    source1.set(1)
    assert(depOfDynamic.now == 101)
    source2.set(200)
    assert(depOfDynamic.now == 201)

    assert(depOfDynamicTracker.values == List(101, 201))
    assert(source2DepTracker.values == List(201))
  }

  @Test
  def addDynamicDependency2Before1(): Unit = {
    LogUtils.log("======")
    source2.set(200)
    assert(depOfDynamic.now == 1)
    source1.set(1)
    assert(depOfDynamic.now == 201)

    assert(depOfDynamicTracker.values == List(201))
    assert(source2DepTracker.values == List(201))
  }

  @Test()
  def addDynamicDependencyParallel2Before1(): Unit = {


    val thread1 = createThread {Thread.sleep(2*letOtherUpdateCreateFramesTime); source1.set(1) }
    val thread2 = createThread {source2.set(200) }

    LogUtils.log("=======")

    thread2.start
    thread1.start
    thread1.join
    thread2.join

    // In any case
    assert(source2DepTracker.values == List(201))

    assert(depOfDynamicTracker.values == List(201))

  }

  @Test()
  def addDynamicDependencyParallel1Before2(): Unit = {


    val thread1 = createThread { source1.set(1) }
    val thread2 = createThread { Thread.sleep(letOtherUpdateCreateFramesTime); source2.set(200) }

    LogUtils.log("====")
    thread2.start
    thread1.start
    thread1.join
    thread2.join

    // In any case
    assert(source2DepTracker.values == List(201))

    // if the change at 1 is before 2, the update at 2 is visible at depOfDynamicDep,
    // otherwise only the end result
    assert(depOfDynamicTracker.values == List(101, 201))

  }



  @Test
  def exisitingTurnsAfterDynamicPropagateToNewNodes(): Unit = {
    // Need to enforce an order between turns that update
    // source1 and source2 when the dynamic dependency has
    // not been established
    // To do that, I connect the paths with a new node
    val source1 = Var(0)
    val source2 = Var(0)
    val dep1 = Signals.static(source1)(implicit t => {Thread.sleep(minEvaluationTimeOfUpdate); source1.get})
    val dynDep1 = engine.dynamic()(implicit t => if (dep1(t) % 2 == 0) dep1(t) else dep1(t) + source2(t))
    val dep2 = Signals.static(source2)(implicit t => source2.get)
    val dep12 = Signals.static(dynDep1, dep2)(implicit t => dynDep1.get + dep2.get)

    val dep1Tracker = new ValueTracker(dep1)
    val dynDep1Tracker = new ValueTracker(dynDep1)
    val dep2Tracker = new ValueTracker(dep2)
    val dep12Tracker = new ValueTracker(dep12)

    val thread1 = createThread {source1.set(1)}
    val thread2 = createThread {Thread.sleep(letOtherUpdateCreateFramesTime); source2.set(100)}

    LogUtils.log("=======")

    thread1.start
    thread2.start
    thread1.join
    thread2.join

    assert(dep1Tracker.values == List(1))
    assert(dynDep1Tracker.values == List(1, 101))
    assert(dep2Tracker.values == List(100))
    assert(dep12Tracker.values == List(1, 201))

  }

}
