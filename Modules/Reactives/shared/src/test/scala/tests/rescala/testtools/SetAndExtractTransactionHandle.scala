package tests.rescala.testtools

import reactives.core.{Initializer, Scheduler}
import reactives.default.global.State as BundleState
import reactives.operator.{Interface, Source}

class SetAndExtractTransactionHandle[Api <: Interface](val api: Api) {
  import api.*
  def SetAndExtractTransactionHandle[A, N](
      source: Source[A] { type State[V] = api.global.State[V] },
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
