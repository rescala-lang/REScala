package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import java.util.concurrent.atomic.AtomicReference
import rescala.graph.TurnFrame
import rescala.pipelining.tests.PipelineTestUtils._
import org.junit.Test
import rescala.pipelining.PipelineEngine
import java.util.concurrent.CyclicBarrier
import rescala.util.JavaFunctionsImplicits._

class FrameAwaitTest extends AssertionsForJUnit with MockitoSugar {

  val engine = new PipelineEngine
  val handledFrames: AtomicReference[List[TurnFrameImpl]] = new AtomicReference(List())
  private object modificationLock

  def markFrameHandled(frame: TurnFrameImpl) = {
    handledFrames.getAndUpdate({ list: List[TurnFrameImpl] => list :+ frame })
  }

  def assertHandledFrames(expectedframes: TurnFrameImpl*) = {
    val frames = handledFrames.get
    assert(frames == expectedframes.toList)
  }

  def createWriteThread(frame: TurnFrameImpl, wait: Int = 0) = {
    createThread {
      frame.awaitPredecessor(modificationLock)
      markFrameHandled(frame)
      frame.markWritten()
    }
  }

  @Test(timeout=500)
  def testFrameWaitsOnOther() = {
    val frame1 = new TurnFrameImpl(engine.makeTurn)
    val frame2 = new TurnFrameImpl(engine.makeTurn)
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
    val frames = List.fill(4)(new TurnFrameImpl(engine.makeTurn))
   
    frames(1).insertAfter(frames(0))
    frames(2).insertAfter(frames(1))
    frames(3).insertAfter(frames(2))

    val threads = frames.map(createWriteThread(_))
    
    List(0,2,3).map(threads(_)).foreach(_.start)
    threads(0).join
    assert(frames(0).isWritten())
    assert(!frames(2).isWritten())
    assert(!frames(3).isWritten())
    
    frames(1).moveAfter(frames(2))
    // After now, the order is frames(0), frames(2), frames(1), frames(3)
    threads(2).join()
    assert(frames(2).isWritten())
    assert(!frames(3).isWritten())
    
    threads(1).start
    threads(1).join
    threads(3).join
    assert(frames(1).isWritten())
    assert(frames(3).isWritten())
    
  }

}