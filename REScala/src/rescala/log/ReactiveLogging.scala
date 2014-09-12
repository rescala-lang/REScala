package rescala.log

trait ReactiveLogging {
  def log: Logging = ReactiveLogging.log
}

object ReactiveLogging {
  var log: Logging = new NoLogging
}
