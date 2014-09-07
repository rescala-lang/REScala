package rescala.log

trait ReactiveLogging {
  def log: Logging = NoLogging
}
