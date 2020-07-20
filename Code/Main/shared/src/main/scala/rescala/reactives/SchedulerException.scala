package rescala.reactives

import rescala.core.{Derived, ReSource, Struct}
import rescala.extra.simpleprop.SimpleStruct

sealed trait SchedulerException extends RuntimeException {
};

case class EvaluationException(t: Throwable, reactive: Derived[SimpleStruct])
  extends SchedulerException {

  override def getMessage: String = {
    s"${t} in reactive ${reactive.name.str}"
  }

}

case class TransactionException(t: EvaluationException, causalErrorChains: Seq[Seq[ReSource[SimpleStruct]]])
  extends SchedulerException {

  override def getMessage: String = {
    val chainErrorMessage = if (causalErrorChains.nonEmpty)
      "The error was caused by these update chains:\n\n" ++ causalErrorChains.map(_.map(_.name.str).mkString("\n↓\n")).mkString("\n---\n")
    else "The error was not triggered by a change."

    s"${t.getMessage}\n$chainErrorMessage\n"
  }
}

case class PipelinedException[S <: Struct](t: Throwable) extends SchedulerException {
  override def getMessage: String = t.getMessage
}
