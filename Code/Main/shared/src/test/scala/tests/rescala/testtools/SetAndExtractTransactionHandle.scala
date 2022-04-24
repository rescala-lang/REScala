package tests.rescala.testtools

import rescala.interface.RescalaInterface

class SetAndExtractTransactionHandle[Api <: RescalaInterface](val api: Api) {
  import api._
  def SetAndExtractTransactionHandle[A, N](source: Source[A], value: A)(implicit engine: Scheduler): Initializer = {
    engine.forceNewTransaction(source) { implicit t =>
      source.admit(value)
      t.tx.initializer
    }
  }
}
