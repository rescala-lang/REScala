package tests.rescala.testtools

import rescala.core.{Initializer, Scheduler}
import rescala.interface.RescalaInterface

class SetAndExtractTransactionHandle[Api <: RescalaInterface](val api: Api) {
  import api._
  def SetAndExtractTransactionHandle[A, N](
      source: Source[A] { type State[V] = api.State[V] },
      value: A
  )(implicit
      engine: Scheduler[State]
  ): Initializer[State] = {
    engine.forceNewTransaction(source) { implicit t =>
      source.admit(value)
      t.tx.initializer
    }
  }
}
