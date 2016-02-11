package tests.rescala.pipelining

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.Frame
import rescala.pipelining.PipelineEngine
import rescala.pipelining.PipeliningTurn
import rescala.turns.Turn
import rescala.Var
import rescala.pipelining.Pipeline

class FrameTest extends AssertionsForJUnit with MockitoSugar {

  implicit val engine = new PipelineEngine
  val dummyReactive = new Pipeline(null)

  @Test
  def testNewWriteFrameIsNotWritten() = {
    assert(Frame(engine.makeTurn, dummyReactive).isWritten == false)
  }

  @Test
  def testMarkWritten() = {
    val frame = Frame(engine.makeTurn, dummyReactive)
    assert(!frame.isWritten)
    frame.markWritten()
    assert(frame.isWritten)
  }

  @Test
  def testInsertFrame() = {
    val frame1 = Frame(engine.makeTurn, dummyReactive)
    val frame2 = Frame(engine.makeTurn, dummyReactive)
    val frame3 = Frame(engine.makeTurn, dummyReactive)

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
    val frame1 = Frame(engine.makeTurn, dummyReactive)
    val frame2 = Frame(engine.makeTurn, dummyReactive)
    val frame3 = Frame(engine.makeTurn, dummyReactive)

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
    val frame1 = Frame(engine.makeTurn, dummyReactive)
    val frame2 = Frame(engine.makeTurn, dummyReactive)
    val frame3 = Frame(engine.makeTurn, dummyReactive)

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