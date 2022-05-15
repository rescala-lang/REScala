package tests.rescala.fullmv.mirrors

import org.scalatest.funsuite.AnyFunSuite
import rescala.fullmv.DistributedFullMVApi.{Event, Evt, FullMVEngine, ReactiveLocalClone, Signal, Var, scopedScheduler}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration

class ReactiveMirrorTest extends AnyFunSuite {
  val engineA = new FullMVEngine(Duration.Zero, "A")
  val engineB = new FullMVEngine(Duration.Zero, "B")
  test("basic mirroring works") {
    val input      = scopedScheduler.withValue(engineA) { Var(5) }
    val reflection = ReactiveLocalClone(input, engineB)

    assert(scopedScheduler.withValue(engineB) { reflection.readValueOnce } === 5);
    scopedScheduler.withValue(engineA) { input.set(123) }
    assert(scopedScheduler.withValue(engineB) { reflection.readValueOnce } === 123)
  }

  test("reflection supports derivations") {
    val input      = scopedScheduler.withValue(engineA) { Var(5) }
    val reflection = ReactiveLocalClone(input, engineB)
    val derived    = scopedScheduler.withValue(engineB) { reflection.map(_ * 2) }

    assert(scopedScheduler.withValue(engineB) { derived.readValueOnce } === 10);
    scopedScheduler.withValue(engineA) { input.set(123) }
    assert(scopedScheduler.withValue(engineB) { derived.readValueOnce } === 246)
  }

  test("reflection maintains glitch freedom") {
    val input      = scopedScheduler.withValue(engineA) { Var(5) }
    val branch1A   = scopedScheduler.withValue(engineA) { input.map("1a" -> _) }
    val branch1B   = scopedScheduler.withValue(engineB) { ReactiveLocalClone(branch1A, engineB).map("1b" -> _) }
    val reflection = ReactiveLocalClone(input, engineB)
    val branch2A   = scopedScheduler.withValue(engineA) { input.map("2a" -> _) }
    val branch2B   = scopedScheduler.withValue(engineB) { ReactiveLocalClone(branch2A, engineB).map("2b" -> _) }

    val tracker = ArrayBuffer[((String, (String, Int)), Int, (String, (String, Int)))]()
    val derived = scopedScheduler.withValue(engineB) {
      Signal {
        tracker.synchronized {
          val v = (branch1B(), reflection(), branch2B())
          tracker += v
          v
        }
      }
    }

    assert(scopedScheduler.withValue(engineB) { derived.readValueOnce } === ((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
    assert(tracker === ArrayBuffer((("1b", ("1a", 5)), 5, ("2b", ("2a", 5)))))
    tracker.clear(); scopedScheduler.withValue(engineA) { input.set(123) }
    assert(scopedScheduler.withValue(engineB) { derived.readValueOnce } === ((
      ("1b", ("1a", 123)),
      123,
      ("2b", ("2a", 123))
    )))
    assert(tracker === ArrayBuffer((("1b", ("1a", 123)), 123, ("2b", ("2a", 123)))))
  }

  test("events work too") {
    val input      = scopedScheduler.withValue(engineA) { Evt[Int]() }
    val branch1A   = scopedScheduler.withValue(engineA) { input.map("1a" -> _) }
    val branch1B   = scopedScheduler.withValue(engineB) { ReactiveLocalClone(branch1A, engineB).map("1b" -> _) }
    val reflection = ReactiveLocalClone(input, engineB)
    val branch2A   = scopedScheduler.withValue(engineA) { input.map("2a" -> _) }
    val branch2B   = scopedScheduler.withValue(engineB) { ReactiveLocalClone(branch2A, engineB).map("2b" -> _) }

    val tracker = ArrayBuffer[(Option[(String, (String, Int))], Option[Int], Option[(String, (String, Int))])]()
    val derived = scopedScheduler.withValue(engineB) {
      Event {
        tracker.synchronized {
          val v = (branch1B(), reflection(), branch2B())
          tracker += v
          Some(v)
        }
      }
    }
    val hold = scopedScheduler.withValue(engineB) { derived.last(1) }

    assert(scopedScheduler.withValue(engineB) { hold.readValueOnce } === List())
    assert(tracker.isEmpty) // in case this failed, someone may have changed event initialization semantics again. Try instead for === ArrayBuffer((None, None, None))
    tracker.clear(); scopedScheduler.withValue(engineA) { input.fire(123) }
    assert(scopedScheduler.withValue(engineB) { hold.readValueOnce } === List((
      Some(("1b", ("1a", 123))),
      Some(123),
      Some(("2b", ("2a", 123)))
    )))
    assert(tracker === ArrayBuffer((Some(("1b", ("1a", 123))), Some(123), Some(("2b", ("2a", 123))))))
  }
}
