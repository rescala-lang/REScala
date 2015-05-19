package rescala.pipelining.tests

import scala.collection.immutable.Queue
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Frame
import rescala.pipelining.PipelineEngine
import rescala.pipelining.PipeliningTurn
import rescala.turns.Turn
import rescala.pipelining.PipelineBuffer
import rescala.graph.Buffer
import rescala.graph.Reactive

class PipelineBufferTest extends AssertionsForJUnit with MockitoSugar {
  
  
  
  val pipelineBuffer= new PipelineBuffer(null)
  val engine = new PipelineEngine
  val buffer  = pipelineBuffer.createBuffer[Int](0, Buffer.commitAsIs, false)

  
  def readStableFrame() = {
    pipelineBuffer.getStableFrame().valueForBuffer(buffer).value.asInstanceOf[Int]
  }
  
  @Test
  def testInitialOnlyStable() = {
    assert(pipelineBuffer.getStableFrame() != null)
    assert(readStableFrame == 0)
    assert(pipelineBuffer.getPipelineFrames().isEmpty)
  }
  
  @Test
  def createFirstFrameHasSameValueAsStableExceptTurn() = {
    implicit val turn = engine.makeTurn
    pipelineBuffer.createFrame
    assert(readStableFrame == 0)
    assert(pipelineBuffer.getPipelineFrames().size == 1)
    val newFrame = pipelineBuffer.getPipelineFrames()(0)
    assert(buffer.get == 0)
    assert(newFrame.turn == turn)
  }
  
  @Test
  def createWriteAndRemoveFrame() = {
    implicit val turn = new PipeliningTurn(engine)
    pipelineBuffer.createFrame
    assert(pipelineBuffer.hasFrame)
    val newFrame = pipelineBuffer.getPipelineFrames()(0)
    buffer.set(1)
    pipelineBuffer.markWritten(turn)
    assert(newFrame.isWritten)
    pipelineBuffer.removeFrame
    assert(pipelineBuffer.getPipelineFrames().isEmpty)
    val stableFrame = pipelineBuffer.getStableFrame()
    assert(readStableFrame == 1)
  }
  
  @Test
  def createMultipleFramesAndRemoveThem() = {
    // Create three frames
    val turn1 = new PipeliningTurn(engine)
    pipelineBuffer.createFrame(turn1)
    val frame1 = pipelineBuffer.needFrame()(turn1)
    assert(!frame1.isWritten)
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1))
    buffer.set(1)(turn1)
    pipelineBuffer.markWritten(turn1)
    assert(frame1.isWritten)
    
    val turn2 = new PipeliningTurn(engine)
    pipelineBuffer.createFrame(turn2)
    val frame2 = pipelineBuffer.needFrame()(turn2)
    assert(buffer.get(turn2) == 1)
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2))
    buffer.set(2)(turn2)
    assert(frame2.turn == turn2)
    pipelineBuffer.markWritten(turn2)
    
    val turn3 = new PipeliningTurn(engine)
    pipelineBuffer.createFrame(turn3)
    val frame3 = pipelineBuffer.needFrame()(turn3)
    assert(pipelineBuffer.getPipelineFrames() == Queue[Frame[_]](frame1, frame2, frame3))
    assert(buffer.get(turn3) == 2)
    buffer.set(3)(turn3)
    
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2, frame3))
    assert(readStableFrame == 0)
    
    // No remove the first one, this removed frame 1 and 2
    pipelineBuffer.removeFrame(turn1)
    pipelineBuffer.removeFrame(turn2)
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame3))
    assert(readStableFrame == 2)
    
    pipelineBuffer.markWritten(turn3)
    
    // Finally remove the last one
    assert(buffer.get(turn3) == 3)
    pipelineBuffer.removeFrame(turn3)
    assert(pipelineBuffer.getPipelineFrames().isEmpty)
    assert(readStableFrame == 3)
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
      pipelineBuffer.createFrame(turn)
      buffer.set(i)(turn)
      pipelineBuffer.markWritten(turn)
    }
    val Queue(frame1, frame2, frame3, frame4, frame5) = pipelineBuffer.getPipelineFrames()

    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2, frame3, frame4, frame5))
    
    var seenFrames : List[Frame[_]] = List()
    pipelineBuffer.foreachFrameTopDown(frame => seenFrames :+= frame)
    
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2, frame3, frame4, frame5))
    assert(seenFrames == List(frame1, frame2, frame3, frame4, frame5))
  }
  
  @Test
  def fillFrame() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    pipelineBuffer.createFrame(turn1)
    pipelineBuffer.createFrame(turn2)
    
    val Queue(frame1, frame2) = pipelineBuffer.getPipelineFrames()
    assert(frame1.turn == turn1)
    assert(frame2.turn == turn2)
    assert(buffer.get(turn1) == 0)
    assert(buffer.get(turn2) == 0)
    
    buffer.set(1)(turn1)
    assert(buffer.get(turn1) == 1)
    assert(buffer.get(turn2) == 0)
    pipelineBuffer.fillFrame(turn2)
    
    val Queue(frame1_, frame2_) = pipelineBuffer.getPipelineFrames()
    assert(frame1 == frame1_)
    assert(frame2_.turn == turn2)
    assert(buffer.get(turn2) == 1)
  }
  
}