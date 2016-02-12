package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Var
import rescala.pipelining.PipelineEngine
import rescala.Signals
import org.junit.Test
import PipelineTestUtils._
import scala.collection.immutable.Queue

class TransitiveResolvingTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit val engine = PipelineEngine
  
  /*
   * This tests uses the following topology which allows three
   * parallel updated with a transitive waiting relation
   * 
   * S1    S2   S3
   * | \  / \  / |
   * |  \/   \/  |
   * |  /\   /\  |
   * | /  \ /  \ |
   * D1   D2    D3
   */
  
  val s1 = Var(0)
  val s2 = Var(0)
  val s3 = Var(0)
  val d1 = Signals.static(s1,s2){implicit t => s1.get - s2.get}
  val d2 = Signals.static(s1,s3){implicit t => s1.get - s3.get}
  val d3 = Signals.static(s2,s3){implicit t => s2.get - s3.get}
  
  @Test
  def resolveTransitiveConflict() = {
   
  }
  

}