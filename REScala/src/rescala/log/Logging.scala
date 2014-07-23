package rescala.log

import rescala.Reactive
import rescala.Stamp

/**
 * Interface for logging.
 */
trait Logging {
  def nodeCreated(r: Reactive): Unit
  def nodeAttached(dependent: Reactive, r: Reactive): Unit
  def nodePulsed(r: Reactive): Unit
  def nodeScheduled(r: Reactive): Unit
  def nodeEvaluationStarted(r: Reactive): Unit
  def nodeEvaluationEnded(r: Reactive): Unit
  def nodePropagationStopped(r: Reactive): Unit
  def logRound(ts: Stamp): Unit
  def logMessage(s: String): Unit
}

/** A Logging stub, that performs no logging */
object NoLogging extends Logging {
  def nodeCreated(r: Reactive): Unit = {}
  def nodeAttached(dependent: Reactive, r: Reactive): Unit = {}
  def nodePulsed(r: Reactive): Unit = {}
  def nodeScheduled(r: Reactive): Unit = {}
  def nodeEvaluationStarted(r: Reactive): Unit = {}
  def nodeEvaluationEnded(r: Reactive): Unit = {}
  def nodePropagationStopped(r: Reactive): Unit = {}
  def logRound(ts: Stamp): Unit = {}
  def logMessage(s: String): Unit = {}
}
