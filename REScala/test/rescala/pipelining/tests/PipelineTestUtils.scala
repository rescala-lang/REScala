package rescala.pipelining.tests

import rescala.graph.Framed
import scala.collection.immutable.Queue
import rescala.turns.Turn

object PipelineTestUtils {

  def frameTurns(f : Framed) : Queue[Turn] = {
    f.getPipelineFrames().map { _.turn}
  }
  
}