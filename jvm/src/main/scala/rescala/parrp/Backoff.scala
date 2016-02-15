package rescala.parrp

class Backoff(var currentBackoff: Long = 100L * 1000L, val maxBackoff: Long = 10L * 1000L * 1000L, val factor: Double = 1.2D) {
  def backoff(): Unit = {
    if (currentBackoff < 1000000L) {
      val start = System.nanoTime()
      while (System.nanoTime() < currentBackoff + start) {Thread.`yield`()}
    }
    else {
      Thread.sleep(currentBackoff / 1000000L)
    }
    currentBackoff = Math.min(Math.round(currentBackoff * factor), maxBackoff)
  }

}
