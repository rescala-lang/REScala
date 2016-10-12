package rescala.pipelining.tests

import java.util.concurrent.CyclicBarrier

import org.scalatest.FlatSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._
import rescala.pipelining.tests.PipelineTestUtils._
import rescala.pipelining.{Pipeline, PipelineEngine, PipeliningTurn}


class FrameWaitWrittenTest extends FlatSpec with TimeLimitedTests {

  trait PipelineState {

    implicit val engine = new PipelineEngine()


    val readFrom = new Pipeline()

    def createNewTurn(): PipeliningTurn = {
      val turn = engine.makeTurn()
      engine.addTurn(turn)
      turn
    }
  }

  override val timeLimit = 100000.millis



  it should "doesNotWaitIfNoFrame" in new PipelineState {
    val readTurn = createNewTurn
    readFrom.waitUntilCanRead(readTurn)
    // No waiting, so nothing to do to stop waiting
  }

  it should "doesNotWaitOnLaterTurn" in new PipelineState {
    val readTurn = createNewTurn
    val frameTurn = createNewTurn
    readFrom.createFrame(frameTurn)
    readFrom.waitUntilCanRead(readTurn)
    // Again nothing to do
  }

  it should "doesWaitOnEarlierTurn" in new PipelineState {
    val frameTurn = createNewTurn
    val readTurn = createNewTurn
    readFrom.createFrame(frameTurn)
    val writeThread = createThread {
      Thread.sleep(500)
      readFrom.markWritten(frameTurn)
    }
    @volatile
    var wasWritten = false;
    val readThread = createThread {
      readFrom.waitUntilCanRead(readTurn)
      wasWritten = readFrom.frame(frameTurn).isWritten
    }
    readThread.start()
    writeThread.start()
    readThread.join()
    writeThread.join()
    assert(wasWritten, "wait Until Can Read does not wait for an earlier turn")
  }

  it should "doesWaitOnEarlierInsertedTurn" in new PipelineState {
    val beginFrameTurn = createNewTurn
    val insertedFrameTurn = createNewTurn
    val readTurn = createNewTurn
    readFrom.createFrame(beginFrameTurn)

    val frameCreatedBarrier = new CyclicBarrier(2)
    val beginWrittenBarrier = new CyclicBarrier(2)

    val beginWriteThread = createThread {
      frameCreatedBarrier.await()
      Thread.sleep(500)
      readFrom.markWritten(beginFrameTurn)
      beginWrittenBarrier.await();
    }

    val insertFrameThread = createThread {
      Thread.sleep(500)
      readFrom.createFrame(insertedFrameTurn)
      frameCreatedBarrier.await()
    }

    val writeInsertedThread = createThread {
      beginWrittenBarrier.await()
      readFrom.markWritten(insertedFrameTurn)
    }

    @volatile
    var wasWritten = false
    val readThread = createThread {
      readFrom.waitUntilCanRead(readTurn)
      wasWritten = readFrom.frame(insertedFrameTurn).isWritten
    }


    val allFrames = List(readThread, writeInsertedThread, insertFrameThread, beginWriteThread)
    allFrames.foreach(_.start)
    allFrames.foreach(_.join)

    assert(wasWritten, "wait Until Can Read does not wait for an inserted but earlier turn")

  }

  it should "doesWaitOnOtherFrameIfWaitingIsDeleted" in new PipelineState {
    val beginFrameTurn = createNewTurn
    val deletedFrameTurn = createNewTurn
    val readTurn = createNewTurn

    readFrom.createFrame(beginFrameTurn)
    readFrom.createFrame(deletedFrameTurn)

    val frameRemovedBarrier = new CyclicBarrier(2)

    val deletedFrameThread = createThread {
      Thread.sleep(500)
      readFrom.deleteFrames(deletedFrameTurn)
      frameRemovedBarrier.await()
    }

    val writeThread = createThread {
      frameRemovedBarrier.await()
      Thread.sleep(500)
      readFrom.markWritten(beginFrameTurn)
    }

    @volatile
    var wasWritten = false
    val readThread = createThread {
      readFrom.waitUntilCanRead(readTurn)
      wasWritten = readFrom.frame(beginFrameTurn).isWritten
    }

    val allThreads = List(readThread, writeThread, deletedFrameThread)
    allThreads.foreach(_.start)
    allThreads.foreach(_.join)

    assert(wasWritten, "wait Until Can Read does not wait if an earlier frame is deleted but no frame remains")
  }

  it should "stopsWaitingIfDependingFrameIsDeleted" in new PipelineState {
    val deletedFrameTurn = createNewTurn()
    val readTurn = createNewTurn()

    readFrom.createFrame(deletedFrameTurn)

    val deleteFrameThread = createThread {
      Thread.sleep(500)
      readFrom.deleteFrames(deletedFrameTurn)
    }

    deleteFrameThread.start

    readFrom.waitUntilCanRead(readTurn)

  }

  it should "doesNotWaitForItself" in new PipelineState {
    val readTurn = createNewTurn
    readFrom.createFrame (readTurn)

    readFrom.waitUntilCanRead(readTurn)
  }

  it should "doesWaitForPreceedingButNotForPostceedingFrames" in new PipelineState {
    val beforeFrameTurn = createNewTurn()
    val readTurn = createNewTurn()
    val afterFrameTurn = createNewTurn()

    readFrom.createFrame(beforeFrameTurn)
    readFrom.createFrame(afterFrameTurn)

    val writeThread = createThread {
      Thread.sleep(500)
      readFrom.markWritten(beforeFrameTurn)
    }

     @volatile
    var wasWritten = false
    val readThread = createThread {
      readFrom.waitUntilCanRead(readTurn)
      wasWritten = readFrom.frame(beforeFrameTurn).isWritten
    }

     writeThread.start
     readThread.start
     writeThread.join
     readThread.join

     assert(wasWritten, "wait Until Can Read does not wait correctly if a postceeding frame is there")

  }

}
