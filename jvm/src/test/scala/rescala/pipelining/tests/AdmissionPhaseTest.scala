package rescala.pipelining.tests

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.PipelineEngine
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.propagation.{Committable, Turn}
import rescala.reactives.Signals

class AdmissionPhaseTest extends AssertionsForJUnit  {

  implicit val engine = new PipelineEngine()
  import engine.Var

  @Test
  def testAdmissionPhaseReadsCorrectValues(): Unit = {
    @volatile var numAdmissions = 0
    val counter = Var(0)
    val numThreads = 20

    val threads = for (_ <- 1 to numThreads) yield createThread {
      engine.plan(counter)(implicit t => {
        val currentValue = counter(t)
        assert(currentValue == numAdmissions)
        counter.admit(currentValue + 1)
        numAdmissions += 1
      })
    }

    threads.foreach { _.start }
    threads.foreach { _.join }

    assert(numAdmissions == numThreads)
    assert(counter.now == numThreads)

  }

  @Test(timeout=100000)
  def testAdmissionPhaseValueMatchesCommitPhaseValue(): Unit = {
    val numThreads = 100

    val counter = Var(0)
    val dep1 = counter.map { _ + 1 }
    val dep2 = counter.map { _ + 2 }
    val dep12 = Signals.lift(dep1, dep2)(_ + _)

    @volatile var threadsOk = true
    @volatile var numAdmissions = 0

    val threads = for( _ <- 1 to numThreads) yield createThread {
      engine.plan(counter)(implicit t => {
        val currentValue = counter(t)
        assert(numAdmissions == currentValue)
        assert(dep1(t) == currentValue +1)
        assert(dep2(t) == currentValue + 2)
        assert(dep12(t) == 2* currentValue +3)
        numAdmissions += 1
        val newValue = currentValue + 1
        counter.admit(newValue)
        t.schedule(new Committable{
          override def release(implicit turn : Turn[_]): Unit = {}
          override def commit(implicit turn : Turn[_]): Unit = {
            val counterVal = counter.get
            val dep1Val = dep1.get
            val dep2Val = dep2.get
            val dep12Val = dep12.get
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
    threads.foreach{_.join}

    assert(counter.now == numThreads)
    assert(threadsOk)
  }

}
