package rescala.pipelining.tests

import java.util.concurrent.atomic.AtomicReference
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Frame
import rescala.pipelining.PipelineEngine
import rescala.pipelining.Pipeline._
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.util.JavaFunctionsImplicits._
import java.util.concurrent.locks.ReentrantLock
import rescala.Var
import rescala.pipelining.PipeliningTurn

class FrameAwaitTest extends AssertionsForJUnit with MockitoSugar {

  implicit val engine = new PipelineEngine
  val handledFrames: AtomicReference[List[Frame[_]]] = new AtomicReference(List())
  private object modificationLock

  val dummyReactive = Var(0)
  
  def markFrameHandled(frame: Frame[_]) = {
    handledFrames.getAndUpdate({ list: List[Frame[_]] => list :+ frame })
  }

  def assertHandledFrames(expectedframes: Frame[_]*) = {
    val frames = handledFrames.get
    assert(frames == expectedframes.toList)
  }

  def createWriteThread(frame: Frame[_], waitingTurn : PipeliningTurn) = {
    createThread {
      frame.awaitPredecessor(modificationLock, waitingTurn)
      markFrameHandled(frame)
      frame.markWritten()
    }
  }

  @Test(timeout = 500)
  def testFrameWaitsOnOther() = {
    val frame1 = Frame(engine.makeTurn,pipelineFor(dummyReactive))
    val frame2 = Frame(engine.makeTurn,pipelineFor(dummyReactive))
    val turn3 = engine.makeTurn
    
    List(frame1.turn, frame2.turn, turn3).foreach { engine.addTurn(_) }
    
    frame2.insertAfter(frame1)

    val frame1Thread = createWriteThread(frame1, frame2.turn)
    val frame2Thread = createWriteThread(frame2, turn3)

    frame2Thread.start()
    frame1Thread.start()

    frame2Thread.join()
    frame1Thread.join()

    assertHandledFrames(frame1, frame2)
  }

  @Test(timeout = 1000)
  def testManyFrameWaitsOnOther() = {

    var waitsOk = true;
    def createFrameThread(frame: Frame[_], waitingTurn : PipeliningTurn) = {
      createThread {
        frame.awaitPredecessor(modificationLock, waitingTurn)
        if (frame.previous() != null)
          if (!frame.previous().isWritten)
            waitsOk = false
        frame.markWritten()
      }
    }

    val numFrames = 100;

    var firstFrame = null.asInstanceOf[Frame[Nothing]]
    var lastFrame = null.asInstanceOf[Frame[Nothing]]
    var threads = List[Thread]()
    for (i <- 1 to 100) {
      val turn = engine.makeTurn
      val newFrame = Frame(turn, pipelineFor(dummyReactive))
      engine.addTurn(turn)
      if (firstFrame != null) {
        newFrame.insertAfter(lastFrame)
        lastFrame = newFrame
      } else {
        firstFrame = newFrame
        lastFrame = firstFrame
      }
    }
    var frame = firstFrame
    while(frame.next() != null) {
      threads +:= createFrameThread(frame, frame.next.turn)
      frame = frame.next().asInstanceOf[Frame[Nothing]]
    }

    threads.foreach { _.start }
    firstFrame.markWritten()
    threads.foreach { _.join }
    
    assert(waitsOk)

  }

  @Test(timeout = 500)
  def testFrameWaitsWithReordering() = {
    val frames = List.fill(4)(Frame(engine.makeTurn, pipelineFor(dummyReactive)))

    frames(1).insertAfter(frames(0))
    frames(2).insertAfter(frames(1))
    frames(3).insertAfter(frames(2))

    val turns = frames.map { _.turn } :+ engine.makeTurn
    turns.foreach { engine.addTurn(_) }
    
    val threads = frames.zip(turns.drop(1)).map(t => createWriteThread(t._1, t._2))

    List(0, 2, 3).map(threads(_)).foreach(_.start)
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