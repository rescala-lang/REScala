package rescala.pipelining.tests

import org.mockito.internal.util.MockitoLogger
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.TurnFrame
import rescala.pipelining.PipeliningTurn
import org.junit.Test
import rescala.pipelining.PipelineEngine
import rescala.turns.Turn

class FrameTest extends AssertionsForJUnit with MockitoSugar  {
  
  val engine = new PipelineEngine
  
 
  
  @Test
  def testRemoveTurn() = {
    val turn = engine.makeTurn
    val frame = new TurnFrameImpl(turn)
    assert(frame.turn == turn)
    frame.removeTurn()
    assert(frame.turn == null)
  }
  
  @Test
  def testNewFrameIsNotWritten() = {
    assert(new TurnFrameImpl(engine.makeTurn).isWritten() == false)
  }
  
  @Test
  def testMarkWritten() = {
    val frame = new TurnFrameImpl(engine.makeTurn)
    assert(!frame.isWritten())
    frame.markWritten()
    assert(frame.isWritten())
  }
  
  @Test
  def testInsertFrame() = {
    val frame1 = new TurnFrameImpl(engine.makeTurn)
    val frame2 = new TurnFrameImpl(engine.makeTurn)
    val frame3 = new TurnFrameImpl(engine.makeTurn)
    
    frame2.insertAfter(frame1)
    
    assert(frame1.previous == null)
    assert(frame1.next == frame2)
    assert(frame2.previous == frame1)
    assert(frame2.next == null)
    
    frame3.insertAfter(frame2)
    
    assert(frame1.previous == null)
    assert(frame1.next == frame2)
    assert(frame2.previous == frame1)
    assert(frame2.next == frame3)
    assert(frame3.previous == frame2)
    assert(frame3.next == null)
  }
  
  @Test
  def testRemoveFrame() = {
    val frame1 = new TurnFrameImpl(engine.makeTurn)
    val frame2 = new TurnFrameImpl(engine.makeTurn)
    val frame3 = new TurnFrameImpl(engine.makeTurn)
    
    frame2.insertAfter(frame1)
    frame3.insertAfter(frame2)
    
    frame2.removeFrame()
    
    assert(frame1.previous == null)
    assert(frame1.next == frame3)
    assert(frame2.previous == null)
    assert(frame2.next == null)
    assert(frame3.previous == frame1)
    assert(frame3.next == null)
    
    frame3.removeFrame()
    
    assert(frame1.previous == null)
    assert(frame1.next == null)
    assert(frame2.previous == null)
    assert(frame2.next == null)
    assert(frame3.previous == null)
    assert(frame3.next == null)
  }
  
  @Test
  def testMoveFrame() = {
    val frame1 = new TurnFrameImpl(engine.makeTurn)
    val frame2 = new TurnFrameImpl(engine.makeTurn)
    val frame3 = new TurnFrameImpl(engine.makeTurn)
    
    frame2.insertAfter(frame1)
    frame3.insertAfter(frame2)
    
    frame1.moveAfter(frame3)
    assert(frame1.previous() == frame3)
    assert(frame3.next() == frame1)
    assert(frame1.next() == null)
    assert(frame2.previous() == null)
    
    frame2.moveAfter(frame3)
    assert(frame3.previous() == null)
    assert(frame3.next() == frame2)
    assert(frame2.previous() == frame3)
    assert(frame2.next() == frame1)
    assert(frame1.previous() == frame2)
  }

}