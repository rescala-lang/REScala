package tests.rescala.util

import rescala.core.{Creation, Scheduler, Struct}
import rescala.reactives.Source

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct](source: Source[A, S], value: A)(implicit engine: Scheduler[S]): Creation[S] = {
    engine.transaction(source) { implicit t =>
      source.admit(value)
      t.cas
    }
  }
}
