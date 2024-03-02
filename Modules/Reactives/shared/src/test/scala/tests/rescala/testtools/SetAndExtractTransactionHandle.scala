package tests.rescala.testtools

import reactives.core.{Initializer, Scheduler}
import reactives.operator.Interface
import reactives.operator.Source
import reactives.default.global.State as BundleState


class SetAndExtractTransactionHandle[Api <: Interface](val api: Api) {
  import api._
  def SetAndExtractTransactionHandle[A, N](
      source: Source[A] { type State[V] = api.global.State[V] },
      value: A
  )(implicit
      engine: Scheduler[BundleState]
  ): Initializer[BundleState] = {
    engine.forceNewTransaction(source) { implicit t =>
      source.admit(value)
      t.tx.initializer
    }
  }
}
