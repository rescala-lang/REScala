package dtn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.util.Using
import java.nio.file.Files
import java.nio.file.Paths
import java.time.ZonedDateTime
import java.time.ZoneId
import java.io.PrintStream
import scala.util.Try

extension [U >: Unit](fut: Future[U])
  def recoverAndLog(): Future[U] = {
    fut.recover(e => {
      Using(Files.newOutputStream(Paths.get(s"/shared/err-${ZonedDateTime.now(ZoneId.of("UTC"))}"))) { out =>
        Using(PrintStream(out)) { outPrinter =>
          e.printStackTrace(outPrinter)
        }
        out.flush()
      }
      e.printStackTrace()
    })
  }

extension [U >: Unit](x: Try[U])
  def recoverAndLog(): Try[U] = {
    x.recover(e => {
      Using(Files.newOutputStream(Paths.get(s"/shared/err-${ZonedDateTime.now(ZoneId.of("UTC"))}"))) { out =>
        Using(PrintStream(out)) { outPrinter =>
          e.printStackTrace(outPrinter)
        }
        out.flush()
      }
      e.printStackTrace()
    })
  }

extension [E <: Exception](e: E)
  def log(): Unit = {
    Using(Files.newOutputStream(Paths.get(s"/shared/err-${ZonedDateTime.now(ZoneId.of("UTC"))}"))) { out =>
      Using(PrintStream(out)) { outPrinter =>
        e.printStackTrace(outPrinter)
      }
      out.flush()
    }
    e.printStackTrace()
  }
