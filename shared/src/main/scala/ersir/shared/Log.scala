package ersir.shared

import de.rmgk.logging.{Level, Logger}

object Log {
  val Log: Logger = de.rmgk.logging.Logger(tag = "", level = Level.Trace, logPrinter = Logger.tracing)
}
