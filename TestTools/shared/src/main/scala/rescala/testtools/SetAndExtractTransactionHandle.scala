package rescala.testhelper

import rescala.core.{AdmissionTicket, Engine, Reactive, Struct, Turn, WrapUpTicket}
import rescala.reactives.Source
;

object SetAndExtractTransactionHandle {
  def apply[A, S <: Struct](source: Source[A, S], value: A)(implicit engine: Engine[S]): Turn[S] = {
    engine.transaction(source) { implicit t =>
      source.admit(value)
      t.creation.asInstanceOf[Turn[S]]
    }
  }

//  import scala.language.existentials
//  def apply[S <: Struct](changes: (Source[A, S], A) forSome { type A }*)(implicit engine: Engine[S]): Turn[S] = {
//    engine.transaction(changes.map(_._1):_*) { implicit t =>
//      def admit[A](change: (Source[A, S], A)) = change._1.admit(change._2)
//      changes.foreach(admit(_))
//      t.creation.asInstanceOf[Turn[S]]
//    }
//  }

  def transaction[R, S <: Struct](initialWrites: Reactive[S]*)(admissionPhase: AdmissionTicket[S] => R)(implicit engine: Engine[S]): (R, Turn[S]) = {
    engine.transactionWithWrapup(initialWrites:_*)(admissionPhase){ (r, t) => (r, t.creation.asInstanceOf[Turn[S]])}
  }

  def transactionWithWrapup[I, R, S <: Struct](initialWrites: Reactive[S]*)(admissionPhase: AdmissionTicket[S] => I)(wrapUpPhase: (I, WrapUpTicket[S]) => R)(implicit engine: Engine[S]): (R, Turn[S]) = {
    engine.transactionWithWrapup(initialWrites:_*)(admissionPhase){ (i, t) => (wrapUpPhase(i, t), t.creation.asInstanceOf[Turn[S]])}
  }
}
