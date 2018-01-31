package tests.rescala.testtools

import rescala.core.{Initializer, Scheduler, Struct}
import rescala.reactives.Source

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct, N](source: Source[S, A], value: A)(implicit engine: Scheduler[S]): Initializer[S] = {
    engine.transaction(source) { implicit t =>
      source.admit(value)
      t.creation
    }
  }
}
