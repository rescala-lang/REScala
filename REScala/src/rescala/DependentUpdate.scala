package rescala

import rescala.synchronization.{Simple, Pessimistic}
import rescala.turns.{Engines, Turn, Engine}

object DependentUpdate {
  def apply[S, R](signal: Signal[S])(condition: S => Boolean)(admissions: Turn => R)(failure: => R): R = {

    implicit class sequentialLeftResult(result: R) {def ~<(sideEffects_! : Unit): R = result }
    val turn = new Pessimistic
    try {
      Engines.currentTurn.withValue(Some(turn)) {

        signal.lock.lock(turn.key)

        val readValue = signal.get(turn)

        if (!condition(readValue)) failure
        else {
          val admissionResult = admissions(turn)

          turn.lockingPhase()
          if (!condition(signal.get(turn))) {
            turn.rollbackPhase()
            failure
          }
          else {
            assert(readValue == signal.get(turn), s"read then act value changed from $readValue to ${ signal.get(turn) }")
            turn.propagationPhase()
            turn.commitPhase()
            admissionResult
          }
        }
      } ~< turn.observerPhase()
    }
    finally {
      turn.realeasePhase()
    }

  }

}

object DependentSynchronizedUpdate {
  def apply[S, R](signal: Signal[S])(condition: S => Boolean)(admissions: Turn => R)(failure: => R): R = Engines.synchron.synchronized {

    implicit class sequentialLeftResult(result: R) {def ~<(sideEffects_! : Unit): R = result }
    val turn = new Simple
    try {
      Engines.currentTurn.withValue(Some(turn)) {

        val readValue = signal.get(turn)

        if (!condition(readValue)) failure
        else {
          val admissionResult = admissions(turn)

          turn.lockingPhase()
          if (!condition(signal.get(turn))) {
            turn.rollbackPhase()
            failure
          }
          else {
            assert(readValue == signal.get(turn), s"read then act value changed from $readValue to ${ signal.get(turn) }")
            turn.propagationPhase()
            turn.commitPhase()
            admissionResult
          }
        }
      } ~< turn.observerPhase()
    }
    finally {
      turn.realeasePhase()
    }

  }

}
