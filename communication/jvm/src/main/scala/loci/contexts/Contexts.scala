package loci
package contexts

import java.util.concurrent.{Executors, ThreadFactory}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.control.NonFatal

object Pooled {
  lazy val global: ExecutionContextExecutor =
    new logging.ReportingExecutionContext(ExecutionContext.global)

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
  lazy val global = create()

  def create(): ExecutionContextExecutor =
    ExecutionContext.fromExecutorService(
      Executors.newSingleThreadExecutor(new ThreadFactory {
        def newThread(runnable: Runnable) = {
          val thread = new Thread(new Runnable {
            def run() =
              try runnable.run()
              catch {
                case NonFatal(exception) =>
                  if (exception.getCause != null)
                    logging.reportException(exception.getCause)
                  else
                    logging.reportException(exception)
              }
          })
          thread.setDaemon(true)
          thread
        }
      }),
      logging.reportException)

  object Implicits {
    implicit lazy val global: ExecutionContext = Queued.global
  }
}
