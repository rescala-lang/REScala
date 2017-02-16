package rescala.pipelining.tests

import java.util.concurrent.{Semaphore, TimeUnit}

import org.scalatest.FlatSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._
import rescala.pipelining.PipelineEngine
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.propagation.Turn
import rescala.reactives.Signals
import rescala.twoversion.Committable

class AdmissionPhaseTest extends FlatSpec with TimeLimitedTests {

  override val timeLimit = 10000.millis

  behavior of s"pipeline admission phase"

  it should "test Admission Phase Reads Correct Values" in {
    implicit val engine = new PipelineEngine()
    import engine.Var

    @volatile var numAdmissions = 0
    val counter = Var(0)
    val numThreads = 20

    val semaphore = new Semaphore(0)

    val threads = for (_ <- 1 to numThreads) yield createThread {
      engine.plan(counter)(implicit t => {
        val currentValue = counter.now(t)
        assert(currentValue == numAdmissions)
        counter.admit(currentValue + 1)
        numAdmissions += 1
      })
      semaphore.release()
    }

    threads.foreach { _.start }
    semaphore.tryAcquire(numThreads, 1, TimeUnit.SECONDS)

    assert(numAdmissions == numThreads)
    assert(counter.now == numThreads)

  }

  it should "test Admission Phase Value Matches Commit Phase Value" in {
    implicit val engine = new PipelineEngine()
    import engine.Var

    val numThreads = 100

    val counter = Var(0)
    val dep1 = counter.map { _ + 1 }
    val dep2 = counter.map { _ + 2 }
    val dep12 = Signals.lift(dep1, dep2)(_ + _)

    @volatile var threadsOk = true
    @volatile var numAdmissions = 0

    val semaphore = new Semaphore(0)


    val threads = for( _ <- 1 to numThreads) yield createThread {
      engine.plan(counter)(implicit t => {
        val currentValue = counter.now(t)
        assert(numAdmissions == currentValue)
        assert(dep1.now(t) == currentValue +1)
        assert(dep2.now(t) == currentValue + 2)
        assert(dep12.now(t) == 2* currentValue +3)
        numAdmissions += 1
        val newValue = currentValue + 1
        counter.admit(newValue)
        t.schedule(new Committable{
          override def release(implicit turn : Turn[_]): Unit = {}
          override def commit(implicit turn : Turn[_]): Unit = {
            val counterVal = counter.now
            val dep1Val = dep1.now
            val dep2Val = dep2.now
            val dep12Val = dep12.now
            try {
            assert(counterVal == newValue)
            assert(dep1Val == newValue + 1)
            assert(dep2Val == newValue + 2)
            assert(dep12Val == 2* newValue + 3)
            } catch {
            case e : Exception => threadsOk = false
            }
          }
        })
      })

    }

    threads.foreach{_.start}
    semaphore.tryAcquire(numThreads, 1, TimeUnit.SECONDS)

    assert(counter.now == numThreads)
    assert(threadsOk)
  }

}
