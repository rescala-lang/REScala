package rescala.testhelper

import rescala.core.{Creation, Engine, Struct}
import rescala.reactives.Source
;

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct](source: Source[A, S], value: A)(implicit engine: Engine[S]): Creation[S] = {
    engine.transaction(source) { implicit t =>
      source.admit(value)
      t.creation
    }
  }
}
