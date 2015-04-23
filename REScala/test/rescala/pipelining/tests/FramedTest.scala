package rescala.pipelining.tests

import scala.collection.immutable.Queue

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import rescala.graph.Frame
import rescala.graph.Framed
import rescala.pipelining.PipelineEngine
import rescala.pipelining.PipeliningTurn
import rescala.turns.Turn

class FramedTest extends AssertionsForJUnit with MockitoSugar {
  
  class TestFrameContent(var num : Int) {
    
    override def toString() = s"Content(num = $num)"
  }
  
  class TestFramed extends Framed {
    
    // Define TestFrame
    protected[this] override type Content = TestFrameContent
    protected[this] def initialStableFrame: Content = new TestFrameContent(0)
    protected[this] def duplicate(other: Content): Content = new TestFrameContent(other.num)
    
    // Make fields accessible in the test
    def getStableFrame() = stableFrame
 /*   override def createFrame (visitFrame: Frame => Boolean = { x: Frame => true }) (implicit turn: Turn) : Unit = 
      super.createFrame(allowedAfterFrame)(turn)
    override def hasFrame(implicit turn: Turn) = super.hasFrame(turn)
    override def removeFrame(implicit turn: Turn) = super.removeFrame(turn)
    
    override def markWritten(implicit turn: Turn) = super.markWritten(turn) */
    def getFrame(implicit turn : Turn) : Frame[Content]= {
      findFrame(_ match {
        case Some(frame) => frame
        case None => fail(s"no frame for turn $turn")
      })
    }
    override def frame[T](f: Content => T = { x: Content => x })(implicit turn: Turn) = super.frame(f)(turn)
  }
  
  val framed = new TestFramed()
  val engine = new PipelineEngine

  @Test
  def testInitialOnlyStable() = {
    assert(framed.getStableFrame() != null)
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
    assert(newFrame.content.num == 0)
    assert(newFrame.turn == turn)
  }
  
  @Test
  def createWriteAndRemoveFrame() = {
    implicit val turn = new PipeliningTurn(engine)
    framed.createFrame()
    assert(framed.hasFrame)
    val newFrame = framed.getPipelineFrames()(0)
    newFrame.content.num =1
    framed.markWritten(turn)
    assert(newFrame.isWritten)
    framed.removeFrame
    assert(framed.getPipelineFrames().isEmpty)
    val stableFrame = framed.getStableFrame()
    assert(stableFrame.num == 1)
  }
  
  @Test
  def createMultipleFramesAndRemoveThem() = {
    // Create three frames
    val turn1 = new PipeliningTurn(engine)
    framed.createFrame()(turn1)
    val frame1 = framed.getFrame(turn1)
    assert(!frame1.isWritten)
    assert(framed.getPipelineFrames() == Queue(frame1))
    frame1.content.num = 1
    framed.markWritten(turn1)
    assert(frame1.isWritten)
    
    val turn2 = new PipeliningTurn(engine)
    framed.createFrame()(turn2)
    val frame2 = framed.getFrame(turn2)
    assert(frame2.content.num == 1)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2))
    frame2.content.num = 2
    assert(frame2.turn == turn2)
    framed.markWritten(turn2)
    
    val turn3 = new PipeliningTurn(engine)
    framed.createFrame()(turn3)
    val frame3 = framed.getFrame(turn3)
    assert(framed.getPipelineFrames() == Queue[Frame[_]](frame1, frame2, frame3))
    assert(frame3.content.num == 2)
    frame3.content.num = 3
    
    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3))
    assert(framed.getStableFrame().num == 0)
    
    // No remove the first one, this removed frame 1 and 2
    framed.removeFrame(turn1)
    framed.removeFrame(turn2)
    assert(framed.getPipelineFrames() == Queue(frame3))
    assert(framed.getStableFrame().num == 2)
    
    framed.markWritten(turn3)
    
    // Finally remove the last one
    assert(frame3.content.num == 3)
    framed.removeFrame(turn3)
    assert(framed.getPipelineFrames().isEmpty)
    assert(framed.getStableFrame().num == 3)
  }
  
  // SUCH BEHVAIOR NOT SUPPORTED BY CURRENT API
  /* @Test
  def createFrameAtSpecificPosition() = {
    // Create five frames
    for (i <- 1 to 5) {
      val turn = new PipeliningTurn(engine)
      framed.createFrame()(turn)
      framed.frame()(turn).num = i
      framed.markWritten(turn)
    }
    val Queue(frame1, frame2, frame3, frame4, frame5) = framed.getPipelineFrames()
    
    // Create a frame which is not allowed to be after a frame with number >= 4
    val turn = new PipeliningTurn(engine)
    framed.createFrame(frame => frame.num < 4)(turn)
    val newFrame = framed.frame()(turn)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3, newFrame, frame4, frame5))
    
  } */
  
  @Test
  def allFramesVisited() = {
    // Create five frames
    for (i <- 1 to 5) {
      val turn = new PipeliningTurn(engine)
      framed.createFrame()(turn)
      framed.frame()(turn).num = i
      framed.markWritten(turn)
    }
    val Queue(frame1, frame2, frame3, frame4, frame5) = framed.getPipelineFrames()

    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3, frame4, frame5))
    
    
    val turn = new PipeliningTurn(engine)
    var seenFrames : List[Frame[TestFrameContent]] = List()
    framed.createFrame(frame => seenFrames = seenFrames :+ frame)(turn)
    val newFrame = framed.getFrame(turn)
    assert(framed.getPipelineFrames() == Queue(frame1, frame2, frame3, frame4, frame5, newFrame))
    assert(seenFrames == List(frame1, frame2, frame3, frame4, frame5))
  }
  
  @Test
  def fillFrame() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    framed.createFrame()(turn1)
    framed.createFrame()(turn2)
    
    val Queue(frame1, frame2) = framed.getPipelineFrames()
    assert(frame1.turn == turn1)
    assert(frame2.turn == turn2)
    assert(frame1.content.num == 0)
    assert(frame2.content.num == 0)
    
    frame1.content.num = 1
    assert(frame1.content.num == 1)
    assert(frame2.content.num == 0)
    framed.fillFrame(turn2)
    
    val Queue(frame1_, frame2_) = framed.getPipelineFrames()
    assert(frame1 == frame1_)
    assert(frame2_.turn == turn2)
    assert(frame2_.content.num == 1)
  }
  
}