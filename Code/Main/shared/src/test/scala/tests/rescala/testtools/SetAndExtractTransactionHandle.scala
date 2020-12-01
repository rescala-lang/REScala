package tests.rescala.testtools

import rescala.core.{Initializer, Scheduler, Struct}
import rescala.operator.Source

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct, N](source: Source[S, A], value: A)(implicit engine: Scheduler[S]): Initializer[S] = {
    engine.forceNewTransaction(source) { implicit t =>
      source.admit(value)
      t.initializer
    }
  }
}
