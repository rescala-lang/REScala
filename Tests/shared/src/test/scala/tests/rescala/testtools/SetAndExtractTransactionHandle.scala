package tests.rescala.testtools

import rescala.core.{Initializer, Scheduler, Struct}
import rescala.reactives.Source

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct](source: Source[A, S], value: A)(implicit engine: Scheduler[S]): Initializer[S] = {
    engine.transaction(source) { implicit t =>
      source.admit(value)
      t.creation
    }
  }
}
