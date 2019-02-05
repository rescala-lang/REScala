package ersir.shared

import de.rmgk.logging.{Level, Logger}

object Log {
  val common: Logger = de.rmgk.logging.Logger(tag = "", level = Level.Debug)
  val Tool: Logger = common.copy(tag = "Tool")
  val Main: Logger = common
  val Devel: Logger = common.copy(tag = "Devel", Level.Trace, Logger.tracing)
  val Narrate: Logger = common.copy(tag = "Narrate")
  val JS: Logger = common.copy(tag = "JS")
  val Crawl: Logger = common.copy(tag = "CW")
  val Store: Logger = common.copy(tag = "IO")
  val Scribe: Logger = common.copy(tag = "Scribe")
  val Server: Logger = common.copy(tag = "Serv")
}
