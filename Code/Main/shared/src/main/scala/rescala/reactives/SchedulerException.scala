package rescala.reactives

import rescala.core.{Derived, ReSource, Struct}

sealed trait SchedulerException extends RuntimeException {
};

case class EvaluationException[S <: Struct](t: Throwable, reactive: Derived[S])
  extends SchedulerException {

  override def getMessage: String = {
    s"${t} in reactive ${reactive.name.str}"
  }

}

case class TransactionException[S <: Struct](t: EvaluationException[S], initialChanges: Set[ReSource[S]])
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
