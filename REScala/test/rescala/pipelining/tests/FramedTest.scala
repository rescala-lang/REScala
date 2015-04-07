package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Framed
import org.junit.Test
import rescala.turns.Turn
import rescala.graph.TurnFrame
import rescala.pipelining.PipeliningTurn
import rescala.pipelining.PipelineEngine
import scala.collection.immutable.Queue

class FramedTest extends AssertionsForJUnit with MockitoSugar {
  
  class TestFrame( _turn : Turn, var num : Int) extends TurnFrame(_turn){
    
    override def toString() = "Frame(num = " + num +")(turn = " + turn + ")"
  }
  
  class TestFramed extends Framed {
    
    // Define TestFrame
    protected[this] override type Frame = TestFrame
    protected[this] def initialStableFrame: Frame = new TestFrame(null, 0)
    protected[this] def newFrameFrom(turn: Turn, other: Frame): Frame = new TestFrame(turn, other.num)
    
    // Make fields accessible in the test
    def getStableFrame() = stableFrame
    override def createFrame (allowedAfterFrame: Frame => Boolean = { x: Frame => true }) (implicit turn: Turn) : Unit = 
      super.createFrame(allowedAfterFrame)(turn)
    override def hasFrame(implicit turn: Turn) = super.hasFrame(turn)
    override def tryRemoveFrame(implicit turn: Turn) = super.tryRemoveFrame(turn)
    override def frame[T](f: Frame => T = { x: Frame => x })(implicit turn: Turn) = super.frame(f)(turn)
    override def markWritten(implicit turn: Turn) = super.markWritten(turn)
  }
  
  val framed = new TestFramed()
  val engine = new PipelineEngine

  @Test
  def testInitialOnlyStable() = {
    assert(framed.getStableFrame() != null)
    assert(framed.getStableFrame().turn == null)
    assert(framed.getStableFrame().num == 0)
    assert(framed.getPipelineFrames().isEmpty)
  }
  
  @Test
  def createFirstFrameHasSameValueAsStableExceptTurn() = {
    implicit val turn = new PipeliningTurn(engine)
    framed.createFrame()
    assert(framed.getStableFrame().num == 0)
    assert(framed.getPipelineFrames().size == 1)
    val newFrame = framed.getPipelineFrames()(0)
    assert(newFrame.num == 0)
    assert(newFrame.turn == turn)
  }
  
  @Test
  def createWriteAndRemoveFrame() = {
    implicit val turn = new PipeliningTurn(engine)
    framed.createFrame()
    assert(framed.hasFrame)
    val newFrame = framed.getPipelineFrames()(0)
    newFrame.num =1
    framed.markWritten(turn)
    assert(newFrame.isWritten())
    framed.tryRemoveFrame
    assert(framed.getPipelineFrames().isEmpty)
    val stableFrame = framed.getStableFrame()
    assert(stableFrame.turn == null)
    assert(stableFrame.num == 1)
  }
  
  @Test
  def createMultipleFramesAndRemoveThem() = {
    // Create three frames
    val turn1 = new PipeliningTurn(engine)
    framed.createFrame()(turn1)
    val frame1 = framed.frame()(turn1)
    assert(!frame1.isWritten())
    assert(framed.getPipelineFrames() == Queue(frame1))
    frame1.num = 1
    framed.markWritten(turn1)
    assert(frame1.isWritten())
    
    val turn2 = new PipeliningTurn(engine)
    framed.createFrame()(turn2)
    val frame2 = framed.frame()(turn2)
    assert(frame2.num == 1)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2))
    frame2.num = 2
    assert(frame2.turn == turn2)
    framed.markWritten(turn2)
    
    val turn3 = new PipeliningTurn(engine)
    framed.createFrame()(turn3)
    val frame3 = framed.frame()(turn3)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3))
    assert(frame3.num == 2)
    frame3.num = 3
    
    // Try remove the second one => should not be removed
    framed.tryRemoveFrame(turn2)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3))
    assert(framed.getStableFrame().num == 0)
    
    // No remove the first one, this removed frame 1 and 2
    framed.tryRemoveFrame(turn1)
    assert(framed.getPipelineFrames() == Queue(frame3))
    assert(framed.getStableFrame().num == 2)
    
    framed.markWritten(turn3)
    
    // Finally remove the last one
    assert(frame3.num == 3)
    framed.tryRemoveFrame(turn3)
    assert(framed.getPipelineFrames().isEmpty)
    assert(framed.getStableFrame().num == 3)
  }
  
  @Test
  def createFrameAtSpecificPosition() = {
    // Create five frames
    for (i <- 1 to 5) {
      val turn = new PipeliningTurn(engine)
      framed.createFrame()(turn)
      framed.frame()(turn).num = i
    }
    val Queue(frame1, frame2, frame3, frame4, frame5) = framed.getPipelineFrames()
    
    // Create a frame which is not allowed to be after a frame with number >= 4
    val turn = new PipeliningTurn(engine)
    framed.createFrame(frame => frame.num < 4)(turn)
    val newFrame = framed.frame()(turn)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3, newFrame, frame4, frame5))
    
  }
  
}