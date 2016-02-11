package tests.rescala.pipelining

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Signal
import rescala.Signals
import rescala.Var
import rescala.graph.Reactive
import rescala.pipelining.PipelineEngine
import tests.rescala.pipelining.PipelineTestUtils._
import rescala.turns.Turn
import rescala.turns.Ticket
import rescala.pipelining.PipeliningTurn
import rescala.pipelining.PipeliningTurn
import rescala.pipelining.PipeliningTurn
import rescala.pipelining.Frame
import rescala.pipelining.Pipeline._

class LinePropagationTest extends AssertionsForJUnit with MockitoSugar {

  //TODO: pipelineEngine is now an object, can no longer be overridden
  /*

  // Modify the pipelined engine to force lock phases to execute sequential:
  // I dont want to test resolving conflicts but the propagation and this
  // gives a deterministic order of the turns with respect to their arrival
  implicit val engine : PipelineEngine = PipelineEngine {
   private val lockPhaseLock = new Object
   override def makeNewTurn = new PipeliningTurn() {
     override def lockPhase(initialWrites: List[Reactive]) = lockPhaseLock.synchronized{
       super.lockPhase(initialWrites)
     }
   }
  }

  */

  /*
   * S
   * |
   * V
   * D1
   * |
   * ..
   * |
   * V
   * DN
   */

  val source = Var((0,0))
  val numNodes = 20
  val deps = createNodes(numNodes, List(source))
  var pipelineOK = true;
 
  var firstTurn = null.asInstanceOf[PipeliningTurn]
  var secondTurn = null.asInstanceOf[PipeliningTurn]
  val waitFirstTurn = 100
  val waitSecondTurn = 10
 
  assert(waitFirstTurn > waitSecondTurn)
  
  def checkPrevTurn(pred : Reactive)(implicit turn : Turn) = {
    def doWithPred(reactive: Reactive, job : Reactive => Unit) = {
      val incomings = reactive.incoming.get(turn)
      if (incomings.nonEmpty)
        job(incomings.head)
    }
      // Next turn should work on node after this node -> the node before the node is finished
    doWithPred(pred, {predpred =>
       pipelineOK &= pipelineFor(predpred).getPipelineFrames().forall { _.isWritten }})
  }

  def createNodes(num: Int, previous: List[Signal[(Int,Int)]]): List[Signal[(Int,Int)]] = {
    def makeNode(other: Signal[(Int,Int)]) = Signals.static(other){implicit t =>
      val (num, wait) = other.get(t)
      if (wait != 0)
        checkPrevTurn(other)(t)
      if (wait == waitFirstTurn) {
        firstTurn = t.asInstanceOf[PipeliningTurn]
      } else if (wait == waitSecondTurn) {
        pipelineOK &= num != 1 ||  !engine.getTurnOrder().contains(firstTurn)
      }
      
        
      Thread.sleep(wait)
      (num + 1,wait)}
    num match {
      case 0 => List()
      case _ =>
        val node = makeNode(previous.last)
        createNodes(num -1, previous :+ node)
    }
  }
  
  @Test
  def testPiplinedPropagation() = {
    // Give turn1 time to begin lock phase before turn2
    val turn1 = createThread{source.set((1,100))}
    val turn2 = createThread{Thread.sleep(50);source.set((2,10))}
    turn1.start
    turn2.start
    
    turn1.join
    turn2.join
    assert(pipelineOK)
  }

}