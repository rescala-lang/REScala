package rescala.reactives

import rescala.core.{Derived, ReSource}
import rescala.extra.simpleprop.SimpleStruct

class InvariantViolationException(t: Throwable, reactive: Derived[SimpleStruct], causalErrorChains: Seq[Seq[ReSource[SimpleStruct]]])
  extends RuntimeException {

  override def getMessage: String = {
    val chainErrorMessage = if (causalErrorChains.nonEmpty)
      "The error was caused by these update chains:\n\n" ++ causalErrorChains.map(_.map(_.name.str).mkString("\n↓\n")).mkString("\n---\n")
    else "The error was not triggered by a change."

    s"${t.getMessage} in reactive ${reactive.name.str}\n$chainErrorMessage\n"
  }

  override def fillInStackTrace() = this
}
