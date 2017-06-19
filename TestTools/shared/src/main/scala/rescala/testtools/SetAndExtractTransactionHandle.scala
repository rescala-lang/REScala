package rescala.testhelper

import rescala.engine.{Engine}
import rescala.graph.{AdmissionTicket, Reactive, Struct, WrapUpTicket}
import rescala.reactives.Source
;

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct](source: Source[A, S], value: A)(implicit engine: Engine[S]): engine.Turn = {
    engine.transaction(source) { implicit t =>
      source.admit(value)
      t.creation.asInstanceOf[engine.Turn]
    }
  }

  import scala.language.existentials
  def apply[S <: Struct](changes: (Source[A, S], A) forSome { type A }*)(implicit engine: Engine[S]): engine.Turn = {
    engine.transaction(changes.map(_._1):_*) { implicit t =>
      def admit[A](change: (Source[A, S], A)) = change._1.admit(change._2)
      changes.foreach(admit(_))
      t.creation.asInstanceOf[engine.Turn]
    }
  }

  def transaction[R, S <: Struct](initialWrites: Reactive[S]*)(admissionPhase: AdmissionTicket[S] => R)(implicit engine: Engine[S]): (R, engine.Turn) = {
    engine.transactionWithWrapup(initialWrites:_*)(admissionPhase){ (r, t) => (r, t.creation.asInstanceOf[engine.Turn])}
  }

  def transactionWithWrapup[I, R, S <: Struct](initialWrites: Reactive[S]*)(admissionPhase: AdmissionTicket[S] => I)(wrapUpPhase: (I, WrapUpTicket[S]) => R)(implicit engine: Engine[S]): (R, engine.Turn) = {
    engine.transactionWithWrapup(initialWrites:_*)(admissionPhase){ (i, t) => (wrapUpPhase(i, t), t.creation.asInstanceOf[engine.Turn])}
  }
}
