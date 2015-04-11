package rescala.pipelining.tests

import rescala.graph.Framed
import scala.collection.immutable.Queue
import rescala.turns.Turn
import scala.util.Random

object PipelineTestUtils {
  
  private val rand = new Random

  def frameTurns(f : Framed) : Queue[Turn] = {
    f.getPipelineFrames().map { _.turn}
  }
  
  def randomWait[A](op: => A) : A = {
    val waitBefore = rand.nextInt(50)
    val waitAfter = rand.nextInt(50)
    Thread.sleep(waitBefore)
    val result = op
    Thread.sleep(waitAfter)
    result
  }
  
}