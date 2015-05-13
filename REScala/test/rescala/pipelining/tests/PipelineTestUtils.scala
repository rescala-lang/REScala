package rescala.pipelining.tests

import scala.collection.immutable.Queue
import rescala.turns.Turn
import scala.util.Random
import rescala.Signal
import rescala.pipelining.PipelineEngine
import rescala.graph.Reactive
import rescala.pipelining.PipelineBuffer

object PipelineTestUtils {
  
  private val rand = new Random

  def frameTurns(f : Reactive) : Queue[Turn] = {
    PipelineBuffer.pipelineFor(f).getPipelineFrames().map { _.turn}
  }
  
  def randomWait[A](op: => A) : A = {
    val waitBefore = rand.nextInt(50)
    val waitAfter = rand.nextInt(50)
    Thread.sleep(waitBefore)
    val result = op
    Thread.sleep(waitAfter)
    result
  }
  
  def createThread(job : => Any) : Thread = {
    new Thread(new Runnable() {
      override def run() = {
        job
      }
    }
    )
  }
  
}

class ValueTracker[T](s : Signal[T])(implicit val engine: PipelineEngine) {
    var values : List[T] = List()
    private object valueLock
    
    s.observe(newValue => valueLock.synchronized{values :+= newValue})
    reset()
    
    def reset() = valueLock.synchronized{values = List()}
  }