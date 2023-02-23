package rescala.extra.invariant

sealed trait InvariantException extends RuntimeException

case class InvariantViolationException(
    t: Throwable,
    reactive: Any,
    causalErrorChains: Seq[Seq[Any]]
) extends InvariantException {

  override val getMessage: String = {
    val chainErrorMessage =
      if (causalErrorChains.nonEmpty)
        "The error was caused by these update chains:\n\n" ++ causalErrorChains.map(_.map(r =>
          s"${r} with value: ${r}"
        ).mkString("\n↓\n")).mkString("\n---\n")
      else "The error was not triggered by a change."

    s"${t.getMessage} in reactive ${reactive}\n$chainErrorMessage\n"
  }

  override def fillInStackTrace(): InvariantViolationException = this
}

case class NoGeneratorException(message: String) extends InvariantException {
  override val getMessage: String = message
}
