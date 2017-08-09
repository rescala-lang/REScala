package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.FullMVEngine._
import rescala.fullmv.transmitter.ReactiveMirror

import scala.collection.mutable.ArrayBuffer

class ReactiveMirrorTest extends FunSuite {
  test("basic mirroring works") {
    val input = Var(5)
    val reflection = ReactiveMirror(input)

    assert(reflection.now == 5)
    input.set(123)
    assert(reflection.now == 123)
  }

  test("reflection supports derivations") {
    val input = Var(5)
    val reflection = ReactiveMirror(input)
    val derived = reflection.map(_ * 2)

    assert(derived.now == 10)
    input.set(123)
    assert(derived.now == 246)
  }

  test("reflection maintains glitch freedom") {
    val input = Var(5)
    val local1 = input.map(1 -> _)
    val reflection = ReactiveMirror(input)
    val local2 = input.map(2 -> _)

    val tracker = ArrayBuffer[((Int, Int), Int, (Int, Int))]()
    val derived = Signal{
      tracker.synchronized {
        val v = (local1(), reflection(), local2())
        tracker += v
        v
      }
    }

    assert(derived.now == ((1 -> 5, 5, 2 -> 5)))
    assert(tracker == ArrayBuffer((1 -> 5, 5, 2 -> 5)))
    tracker.clear()

    input.set(123)
    assert(derived.now == ((1 -> 123, 123, 2 -> 123)))
    assert(tracker == ArrayBuffer((1 -> 123, 123, 2 -> 123)))
  }

  test("events work too") {
    val input = Var[Int](5)
    val local1 = input.map(1 -> _)
    val reflection = ReactiveMirror(input.changed)
    val local2 = input.map(2 -> _)

    val tracker = ArrayBuffer[((Int, Int), Int, (Int, Int))]()
    val derived = Event {
      tracker.synchronized {
        val v = (local1(), reflection().get, local2())
        tracker += v
        Some(v)
      }
    }
    val hold = derived.last(1)

    assert(hold.now == List())
    assert(tracker.isEmpty)

    input.set(123)
    assert(hold.now == List((1 -> 123, 123, 2 -> 123)))
    assert(tracker == ArrayBuffer((1 -> 123, 123, 2 -> 123)))
  }
}
