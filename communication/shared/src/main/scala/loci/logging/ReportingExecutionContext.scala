package loci
package logging

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.control.NonFatal

class ReportingExecutionContext(context: ExecutionContext) extends ExecutionContextExecutor {
  def execute(runnable: Runnable) = context.execute(new Runnable {
    def run() =
      try runnable.run()
      catch { case NonFatal(exception) => reportFailure(exception) }
  })

  def reportFailure(cause: Throwable) = reportException(cause)
}
