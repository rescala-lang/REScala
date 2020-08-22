package rescala.extra.invariant

import rescala.core.{Pulse, ReSource}

class InvariantViolatingGeneratorException(reactive: ReSource[SimpleStruct], invariant: Invariant[_], pulse: Pulse[_])
    extends RuntimeException {

  override def getMessage: String =
    s"The generator for reactive\n\t${reactive.name}\ncreated value \n\t${pulse.get}" +
      s"\nwhich violated invariant\n\t${invariant.description}\n\nPlease make sure Generators create only valid values."

  override def fillInStackTrace() = this
}
