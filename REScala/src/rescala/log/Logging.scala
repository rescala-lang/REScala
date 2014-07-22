package rescala.log

import rescala.Reactive
import rescala.Stamp

/**
 * Interface for logging.
 */
trait Logging {
  def nodeCreated(r: Reactive)
  def nodeAttached(dependent: Reactive, r: Reactive)
  def nodePulsed(r: Reactive)
  def nodeScheduled(r: Reactive)
  def nodeEvaluationStarted(r: Reactive)
  def nodeEvaluationEnded(r: Reactive)
  def nodePropagationStopped(r: Reactive)
  def logRound(ts: Stamp)
  def logMessage(s: String)
}

/** A Logging stub, that performs no logging */
object NoLogging extends Logging {
  def nodeCreated(r: Reactive) {}
  def nodeAttached(dependent: Reactive, r: Reactive) {}
  def nodePulsed(r: Reactive) {}
  def nodeScheduled(r: Reactive) {}
  def nodeEvaluationStarted(r: Reactive) {}
  def nodeEvaluationEnded(r: Reactive) {}
  def nodePropagationStopped(r: Reactive) {}
  def logRound(ts: Stamp) {}
  def logMessage(s: String) {}
}
