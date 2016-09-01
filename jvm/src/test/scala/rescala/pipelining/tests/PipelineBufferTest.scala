package rescala.pipelining.tests

import org.scalatest.FlatSpec
import rescala.pipelining.{Frame, Pipeline, PipelineEngine, PipeliningTurn}

import scala.collection.immutable.Queue
import rescala.pipelining.Buffer

class PipelineBufferTest extends FlatSpec {

  trait PipelineState {

    val pipelineBuffer = new Pipeline()
    val engine = new PipelineEngine()
    val buffer = pipelineBuffer.createNonblockingBuffer[Int](0, Buffer.commitAsIs)

    def createTurn(): PipeliningTurn = {
      val turn = engine.makeTurn
      engine.addTurn(turn)
      turn
    }

    def readStableFrame(): Int = {
      pipelineBuffer.getStableFrame().content.valueForBuffer(buffer).value
    }
  }

  it should "test Initial Only Stable" in new PipelineState {
    assert(pipelineBuffer.getStableFrame() != null)
    assert(readStableFrame == 0)
    assert(pipelineBuffer.getPipelineFrames().isEmpty)
  }

  it should "create First Frame Has Same Value As Stable Except Turn" in new PipelineState {
    implicit val turn = createTurn()
    pipelineBuffer.createFrame
    assert(readStableFrame == 0)
    assert(pipelineBuffer.getPipelineFrames().size == 1)
    val newFrame = pipelineBuffer.getPipelineFrames()(0)
    assert(buffer.get == 0)
    assert(newFrame.turn == turn)
  }

  it should "create Write And Remove Frame" in new PipelineState {
    implicit val turn = createTurn()
    pipelineBuffer.createFrame
    assert(pipelineBuffer.hasFrame)
    val newFrame = pipelineBuffer.getPipelineFrames()(0)
    buffer.set(1)
    pipelineBuffer.markWritten(turn)
    assert(newFrame.isWritten)
    pipelineBuffer.removeFrames
    assert(pipelineBuffer.getPipelineFrames().isEmpty)
    val stableFrame = pipelineBuffer.getStableFrame()
    assert(readStableFrame == 1)
  }

  it should "create Multiple Frames And Remove Them" in new PipelineState {
    // Create three frames
    val turn1 = createTurn()
    pipelineBuffer.createFrame(turn1)
    val frame1 = pipelineBuffer.needFrame()(turn1)
    assert(!frame1.isWritten)
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1))
    buffer.set(1)(turn1)
    pipelineBuffer.markWritten(turn1)
    assert(frame1.isWritten)

    val turn2 = createTurn()
    pipelineBuffer.createFrame(turn2)
    val frame2 = pipelineBuffer.needFrame()(turn2)
    assert(buffer.get(turn2) == 1)
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2))
    buffer.set(2)(turn2)
    assert(frame2.turn == turn2)
    pipelineBuffer.markWritten(turn2)

    val turn3 = createTurn()
    pipelineBuffer.createFrame(turn3)
    val frame3 = pipelineBuffer.needFrame()(turn3)
    assert(pipelineBuffer.getPipelineFrames() == Queue[Frame[_]](frame1, frame2, frame3))
    assert(buffer.get(turn3) == 2)
    buffer.set(3)(turn3)

    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2, frame3))
    assert(readStableFrame == 0)

    // No remove the first one, this removed frame 1 and 2
    pipelineBuffer.removeFrames(turn1)
    pipelineBuffer.removeFrames(turn2)
    assert(pipelineBuffer.getPipelineFrames() == Queue(frame3))
    assert(readStableFrame == 2)

    pipelineBuffer.markWritten(turn3)

    // Finally remove the last one
    assert(buffer.get(turn3) == 3)
    pipelineBuffer.removeFrames(turn3)
    assert(pipelineBuffer.getPipelineFrames().isEmpty)
    assert(readStableFrame == 3)
  }

  // SUCH BEHVAIOR NOT SUPPORTED BY CURRENT API
  /*   it should "createFrameAtSpecificPosition" in {
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

  it should "all Frames Visited" in new PipelineState {
    // Create five frames
    for (i <- 1 to 5) {
      val turn = createTurn()
      pipelineBuffer.createFrame(turn)
      buffer.set(i)(turn)
      pipelineBuffer.markWritten(turn)
    }
    val Queue(frame1, frame2, frame3, frame4, frame5) = pipelineBuffer.getPipelineFrames()

    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2, frame3, frame4, frame5))

    var seenFrames: List[Frame[_]] = List()
    pipelineBuffer.foreachFrameTopDown(frame => seenFrames :+= frame)

    assert(pipelineBuffer.getPipelineFrames() == Queue(frame1, frame2, frame3, frame4, frame5))
    assert(seenFrames == List(frame1, frame2, frame3, frame4, frame5))
  }

}
