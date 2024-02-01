package loci
package contexts

import org.scalajs.macrotaskexecutor.MacrotaskExecutor

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.control.NonFatal

object Pooled {
  lazy val global: ExecutionContextExecutor =
    new logging.ReportingExecutionContext(MacrotaskExecutor)

  object Implicits {
    implicit lazy val global: ExecutionContext = Pooled.global
  }
}

object Immediate {
  lazy val global: ExecutionContextExecutor = new ExecutionContextExecutor {
    def execute(runnable: Runnable) =
      try runnable.run()
      catch { case NonFatal(exception) => reportFailure(exception) }

    def reportFailure(throwable: Throwable) = logging.reportException(throwable)
  }

  object Implicits {
    implicit lazy val global: ExecutionContext = Immediate.global
  }
}

object Queued {
  lazy val global: ExecutionContextExecutor =
    new logging.ReportingExecutionContext(MacrotaskExecutor)

  def create() = global

  object Implicits {
    implicit lazy val global: ExecutionContext = Queued.global
  }
}
