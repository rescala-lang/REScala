package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Var
import rescala.Signals
import rescala.pipelining.PipelineEngine
import org.junit.Test
import rescala.pipelining.PipeliningTurn.lockPhaseLock
import rescala.graph.Reactive
import rescala.pipelining.PipeliningTurn
import rescala.pipelining.tests.PipelineTestUtils._
import java.util.concurrent.CyclicBarrier
import rescala.Signal

/**
 * @author moritzlichter
 */
class DynamicReadTest extends AssertionsForJUnit with MockitoSugar {

  implicit val engine = new PipelineEngine
  
  val minEvaluationTimeOfUpdate = 500
  val letOtherUpdateCreateFramesTime = 100
  
  assert(letOtherUpdateCreateFramesTime < minEvaluationTimeOfUpdate) // Such that the frames are still there
  println
  println
  
  val source1 = Var(0)
  val source2 = Var(100)

  val dynamicDep = Signals.dynamic()(implicit t => {
    println(s"$t: Evaluate base ${source1(t)}")
    if (source1(t) % 2 != 0)
      source2(t)
    else 0
  })
  val depOfDynamic = Signals.static(dynamicDep)(implicit t => {
    Thread.sleep(minEvaluationTimeOfUpdate)
    println(s"$t: Eval of dyn dep completed")
    dynamicDep.get + 1
  })

  val source2Dep = Signals.static(source2)(implicit t => {
    Thread.sleep(minEvaluationTimeOfUpdate)
    println(s"$t: Eval of source2 dep completed")
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
  var depOfDynamicDepValues: List[Int] = List()
  var source2DepValues: List[Int] = List()

  depOfDynamic.observe { newValue => depOfDynamicDepValues :+= newValue }
  source2Dep.observe { newValue => source2DepValues :+= newValue }

  depOfDynamicDepValues = List()
  source2DepValues = List()

  @Test
  def addDynamicDependency1Before2() = {
    println("======")
    source1.set(1)
    assert(depOfDynamic.now == 101)
    source2.set(200)
    assert(depOfDynamic.now == 201)

    assert(depOfDynamicDepValues == List(101, 201))
    assert(source2DepValues == List(201))
  }
  
  @Test
  def addDynamicDependency2Before1() = {
    println("======")
    source2.set(200)
    assert(depOfDynamic.now == 1)
    source1.set(1)
    assert(depOfDynamic.now == 201)

    assert(depOfDynamicDepValues == List(201))
    assert(source2DepValues == List(201))
  }

  @Test()
  def addDynamicDependencyParallel2Before1() = {


    val thread1 = createThread {Thread.sleep(letOtherUpdateCreateFramesTime); source1.set(1) }
    val thread2 = createThread {source2.set(200) }
    
    println("=======")

    thread1.start
    thread2.start
    thread1.join
    thread2.join

    // In any case
    assert(source2DepValues == List(201))

    // if the change at 1 is before 2, the update at 2 is visible at depOfDynamicDep,
    // otherwise only the end result
    val resultSource2BeforeSource1 = List(201)
    println(depOfDynamicDepValues)
    assert(depOfDynamicDepValues == resultSource2BeforeSource1)

  }

  @Test()
  def addDynamicDependencyParallel1Before2() = {


    val thread1 = createThread { source1.set(1) }
    val thread2 = createThread { Thread.sleep(letOtherUpdateCreateFramesTime); source2.set(200) }

    println("====")
    thread2.start
    thread1.start
    thread1.join
    thread2.join

    // In any case
    assert(source2DepValues == List(201))

    // if the change at 1 is before 2, the update at 2 is visible at depOfDynamicDep,
    // otherwise only the end result
    val resultSource1BeforeSource2 = List(101, 201)
    println(depOfDynamicDepValues)
    assert(depOfDynamicDepValues == resultSource1BeforeSource2)

  }
  
  class ValueTracker[T](s : Signal[T]) {
    var values : List[T] = List()
    
    s.observe(newValue => values :+= newValue)
    reset()
    
    def reset() = values = List()
  }
  
  @Test
  def exisitingTurnsAfterDynamicPropagateToNewNodes() = {
    // Need to enforce an order between turns that update
    // source1 and source2 when the dynamic dependency has
    // not been established
    // To do that, I connect the paths with a new node
    val source1 = Var(0)
    val source2 = Var(0)
    val dep1 = Signals.static(source1)(implicit t => {Thread.sleep(minEvaluationTimeOfUpdate); source1.get})
    val dynDep1 = Signals.dynamic()(implicit t => if (dep1(t) % 2 == 0) dep1(t) else dep1(t) + source2(t))
    val dep2 = Signals.static(source2)(implicit t => source2.get)
    val dep12 = Signals.static(dynDep1, dep2)(implicit t => dynDep1.get + dep2.get)
    
    val dep1Tracker = new ValueTracker(dep1)
    val dynDep1Tracker = new ValueTracker(dynDep1)
    val dep2Tracker = new ValueTracker(dep2)
    val dep12Tracker = new ValueTracker(dep12)
    
    val thread1 = createThread {source1.set(1)}
    val thread2 = createThread {Thread.sleep(letOtherUpdateCreateFramesTime); source2.set(100)}
    
    println("=======")
    
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