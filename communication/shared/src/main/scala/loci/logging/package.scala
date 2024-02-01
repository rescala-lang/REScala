package loci

import scribe.{Level, LogRecord, Logger, LoggerSupport}

package object logging extends LoggerSupport[Unit] {
  root.orphan().withHandler(minimumLevel = Some(Level.Info)).replace()

  def root = Logger("scala-loci")

  val reportException: Throwable => Unit = exception =>
    error("unhandled exception", exception)

  def log(record: LogRecord): Unit = {
    val logRecord =
      if (record.className startsWith "loci.logging")
        record.copy(className = "scala-loci", methodName = None, line = None, column = None)
      else
        record.copy(className = record.className.replaceAll("\\.\\$anon[^.]*", "").replaceAll("\\.<.*>", ""))

    val logger = Logger.get(logRecord.className) getOrElse {
      Logger().withParent(root).replace(Some(logRecord.className))
    }

    logger.log(logRecord)
  }
}
