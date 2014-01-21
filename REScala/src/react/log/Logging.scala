package react.log

/**
 * Highly abstract interface for logging.
 * 
 * @param event The type of the event to log. Supported events in "REScalaLogging":
 * LogMessage, LogCreateNode, LogAttachNode, LogScheduleNode, LogPulseNode, 
 * LogStartEvalNode, LogEndEvalNode, LogRound, LogStopPropagation
 */
abstract class Logging {
  def log(event: String, params: Object*)
}

/** A Logging stub, that performs no logging */
object NoLogging extends Logging {
  def log(event: String, params: Object*) {}
}