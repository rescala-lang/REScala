package rescala.parrp

class Backoff(
    val initialBackoff: Long = 100L * 1000L,
    val maxBackoff: Long = 10L * 1000L * 1000L,
    val factor: Double = 1.2d
) {
  var currentBackoff  = initialBackoff
  def backoff(): Unit = Backoff.milliSleepNanoSpin(getAndIncrementBackoff())
  def getAndIncrementBackoff(): Long = {
    val res = currentBackoff
    currentBackoff = Math.min(Math.round(currentBackoff * factor), maxBackoff)
    res
  }
  def reset(): Unit = {
    currentBackoff = initialBackoff
  }
}

object Backoff {
  def milliSleepNanoSpin(backoff: Long): Unit = {
    if (backoff < 1000000L) {
      val start = System.nanoTime()
      while (System.nanoTime() < backoff + start) { Thread.`yield`() }
    } else {
      Thread.sleep(backoff / 1000000L)
    }
  }
}
