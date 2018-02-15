package tests.rescala.fullmv.mirrors

import org.scalatest.FunSuite
import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors.localcloning.ReactiveLocalClone

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration

class ReactiveMirrorTest extends FunSuite {
  val engineA = new FullMVEngine(Duration.Zero, "A")
  val engineB = new FullMVEngine(Duration.Zero, "B")
  test("basic mirroring works") {
    val input = {import engineA._; Var(5)}
    val reflection = ReactiveLocalClone(input, engineB)

    assert({import engineB._; reflection.now} === 5)
    ;{import engineA._; input.set(123)}
    assert({import engineB._; reflection.now} === 123)
  }

  test("reflection supports derivations") {
    val input = {import engineA._; Var(5)}
    val reflection = ReactiveLocalClone(input, engineB)
    val derived = {import engineB._; reflection.map(_ * 2)}

    assert({import engineB._; derived.now} === 10)
    ;{import engineA._; input.set(123)}
    assert({import engineB._; derived.now} === 246)
  }

  test("reflection maintains glitch freedom") {
    val input = {import engineA._; Var(5)}
    val branch1A = {import engineA._; input.map("1a" -> _)}
    val branch1B = {import engineB._; ReactiveLocalClone(branch1A, engineB).map("1b" -> _)}
    val reflection = ReactiveLocalClone(input, engineB)
    val branch2A = {import engineA._; input.map("2a" -> _)}
    val branch2B = {import engineB._; ReactiveLocalClone(branch2A, engineB).map("2b" -> _)}

    val tracker = ArrayBuffer[((String, (String, Int)), Int, (String, (String, Int)))]()
    val derived = {import engineB._; Signal {
      tracker.synchronized {
        val v = (branch1B(), reflection(), branch2B())
        tracker += v
        v
      }
    }}

    assert({import engineB._; derived.now} === ((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
    assert(tracker === ArrayBuffer((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
    tracker.clear()

    ;{import engineA._; input.set(123)}
    assert({import engineB._; derived.now} === ((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
    assert(tracker === ArrayBuffer((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
  }

  test("events work too") {
    val input = {import engineA._; Evt[Int]()}
    val branch1A = {import engineA._; input.map("1a" -> _)}
    val branch1B = {import engineB._; ReactiveLocalClone(branch1A, engineB).map("1b" -> _)}
    val reflection = ReactiveLocalClone(input, engineB)
    val branch2A = {import engineA._; input.map("2a" -> _)}
    val branch2B = {import engineB._; ReactiveLocalClone(branch2A, engineB).map("2b" -> _)}

    val tracker = ArrayBuffer[(Option[(String, (String, Int))], Option[Int], Option[(String, (String, Int))])]()
    val derived = {import engineB._; Event {
      tracker.synchronized {
        val v = (branch1B(), reflection(), branch2B())
        tracker += v
        Some(v)
      }
    }}
    val hold = {import engineB._; derived.last(1)}

    assert({import engineB._; hold.now} === List())
    assert(tracker.isEmpty) // in case this failed, someone may have changed event initialization semantics again. Try instead for === ArrayBuffer((None, None, None))
    tracker.clear()

    ;{import engineA._; input.fire(123)}
    assert({import engineB._; hold.now} === List((Some(("1b", ("1a", 123))), Some(123), Some(("2b", ("2a", 123))))))
    assert(tracker === ArrayBuffer((Some(("1b", ("1a", 123))), Some(123), Some(("2b", ("2a", 123))))))
  }
}
