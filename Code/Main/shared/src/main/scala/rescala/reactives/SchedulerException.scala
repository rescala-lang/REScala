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

case class TransactionException(t: EvaluationException, initialChanges: Set[ReSource[SimpleStruct]])
  extends SchedulerException {

  override def getMessage: String = {
    val changeSet: String = if(initialChanges.nonEmpty) {
      initialChanges.map(re => re.name.str).fold("") {(msg, re) => s"$msg\n\t$re"}
    } else {
      "Empty"
    }
    s"${t.getMessage} with changeSet [$changeSet]"
  }
}

case class PipelinedException[S <: Struct](t: Throwable) extends SchedulerException {
  override def getMessage: String = t.getMessage
}
