package rescala.pipelining.tests

import java.util.concurrent.atomic.AtomicReference

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import rescala.graph.Frame
import rescala.graph.WriteFrame
import rescala.pipelining.PipelineEngine
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.util.JavaFunctionsImplicits._

class FrameAwaitTest extends AssertionsForJUnit with MockitoSugar {
  

  val engine = new PipelineEngine
  val handledFrames: AtomicReference[List[Frame[_]]] = new AtomicReference(List())
  private object modificationLock

  def markFrameHandled(frame: Frame[_]) = {
    handledFrames.getAndUpdate({ list: List[Frame[_]] => list :+ frame })
  }

  def assertHandledFrames(expectedframes: Frame[_]*) = {
    val frames = handledFrames.get
    assert(frames == expectedframes.toList)
  }

  def createWriteThread(frame: WriteFrame[_], wait: Int = 0) = {
    createThread {
      frame.awaitPredecessor(modificationLock)
      markFrameHandled(frame)
      frame.markWritten()
    }
  }

  @Test(timeout=500)
  def testFrameWaitsOnOther() = {
    val frame1 = new WriteFrame(engine.makeTurn)
    val frame2 = new WriteFrame(engine.makeTurn)
    frame2.insertAfter(frame1)

    val frame1Thread = createWriteThread(frame1, 50)
    val frame2Thread = createWriteThread(frame2)

    frame2Thread.start()
    frame1Thread.start()

    frame2Thread.join()
    frame1Thread.join()

    assertHandledFrames(frame1, frame2)
  }

  @Test(timeout=500)
  def testFrameWaitsWithReordering() = {
    val frames = List.fill(4)(new WriteFrame(engine.makeTurn))
   
    frames(1).insertAfter(frames(0))
    frames(2).insertAfter(frames(1))
    frames(3).insertAfter(frames(2))

    val threads = frames.map(createWriteThread(_))
    
    List(0,2,3).map(threads(_)).foreach(_.start)
    threads(0).join
    assert(frames(0).isWritten)
    assert(!frames(2).isWritten)
    assert(!frames(3).isWritten)
    
    frames(1).moveAfter(frames(2))
    // After now, the order is frames(0), frames(2), frames(1), frames(3)
    threads(2).join()
    assert(frames(2).isWritten)
    assert(!frames(3).isWritten)
    
    threads(1).start
    threads(1).join
    threads(3).join
    assert(frames(1).isWritten)
    assert(frames(3).isWritten)
    
  }

}