package rescala.pipelining.tests

import org.scalatest.FlatSpec
import rescala.pipelining.{Frame, Pipeline, PipelineEngine}

class FrameTest extends FlatSpec  {

  implicit val engine = new PipelineEngine()
  val dummyReactive = new Pipeline()

    it should "test New Write Frame Is Not Written" in {
    assert(Frame(engine.makeTurn(), dummyReactive).isWritten == false)
  }

    it should "test Mark Written" in {
    val frame = Frame(engine.makeTurn(), dummyReactive)
    assert(!frame.isWritten)
    frame.markWritten()
    assert(frame.isWritten)
  }

    it should "test Insert Frame" in {
    val frame1 = Frame(engine.makeTurn(), dummyReactive)
    val frame2 = Frame(engine.makeTurn(), dummyReactive)
    val frame3 = Frame(engine.makeTurn(), dummyReactive)

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

    it should "test Remove Frame" in {
    val frame1 = Frame(engine.makeTurn(), dummyReactive)
    val frame2 = Frame(engine.makeTurn(), dummyReactive)
    val frame3 = Frame(engine.makeTurn(), dummyReactive)

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

    it should "test Move Frame" in {
    val frame1 = Frame(engine.makeTurn(), dummyReactive)
    val frame2 = Frame(engine.makeTurn(), dummyReactive)
    val frame3 = Frame(engine.makeTurn(), dummyReactive)

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
