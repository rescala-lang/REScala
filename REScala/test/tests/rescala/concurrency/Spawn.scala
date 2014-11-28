package tests.rescala.concurrency

object Spawn {
  def apply(f: => Unit): Thread = {
    val t = new Thread(new Runnable {
      override def run(): Unit = f
    })
    t.start()
    t
  }
}
