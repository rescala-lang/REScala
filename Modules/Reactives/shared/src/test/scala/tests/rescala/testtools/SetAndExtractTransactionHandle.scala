package tests.rescala.testtools

import reactives.SelectedScheduler.candidate.State as BundleState
import reactives.core.{Initializer, Scheduler}
import reactives.operator.Source

class SetAndExtractTransactionHandle(val api: reactives.default.type) {
  import api.*
  def SetAndExtractTransactionHandle[A, N](
      source: Source[A] { type State[V] = reactives.SelectedScheduler.State[V] },
      value: A
  )(using
      engine: Scheduler[BundleState]
  ): Initializer[BundleState] = {
    engine.forceNewTransaction(source) { implicit t =>
      source.admit(value)
      t.tx.initializer
    }
  }
}
