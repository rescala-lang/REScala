package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.Pipeline
import rescala.Var
import rescala.pipelining.PipelineEngine
import org.junit.Test
import PipelineTestUtils._
import java.util.concurrent.CyclicBarrier

class FrameWaitWrittenTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit val engine = new PipelineEngine()
  
  
  val readFrom = new Pipeline()
  
  def createNewTurn() = {
    val turn = engine.makeTurn
    engine.addTurn(turn)
    turn
  }
  
  @Test(timeout = 100)
  def doesNotWaitIfNoFrame() = {
    val readTurn = createNewTurn
    readFrom.waitUntilCanRead(readTurn)
    // No waiting, so nothing to do to stop waiting
  }
  
  @Test(timeout = 100)
  def doesNotWaitOnLaterTurn() = {
    val readTurn = createNewTurn
    val frameTurn = createNewTurn
    readFrom.createFrame(frameTurn)
    readFrom.waitUntilCanRead(readTurn)
    // Again nothing to do
  }
  
  @Test(timeout = 1000)
  def doesWaitOnEarlierTurn() = {
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
    assert(wasWritten, "waitUntilCanRead does not wait for an earlier turn")
  }
  
  @Test(timeout = 2000)
  def doesWaitOnEarlierInsertedTurn() = {
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
    
    assert(wasWritten, "waitUntilCanRead does not wait for an inserted but earlier turn")
    
  }
  
  @Test(timeout = 2000)
  def doesWaitOnOtherFrameIfWaitingIsDeleted() = {
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
    
    assert(wasWritten, "waitUntilCanRead does not wait if an earlier frame is deleted but no frame remains")
  }
  
  @Test(timeout = 2000)
  def stopsWaitingIfDependingFrameIsDeleted() = {
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
  
  @Test(timeout = 1000)
  def doesNotWaitForItself() = {
    val readTurn = createNewTurn
    readFrom.createFrame (readTurn)
    
    readFrom.waitUntilCanRead(readTurn)
  }
  
  @Test(timeout = 1000)
  def doesWaitForPreceedingButNotForPostceedingFrames() = {
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
     
     assert(wasWritten, "waitUntilCanRead does not wait correctly if a postceeding frame is there")
    
  }

}