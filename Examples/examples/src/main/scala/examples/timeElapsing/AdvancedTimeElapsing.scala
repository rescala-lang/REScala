package examples.timeElapsing

import rescala._


object AdvancedTimeElapsing extends App {

  println("start!")

  val tick = Evt[Unit]

  val numTics = tick.count
  val seconds = Signal {numTics() % 60}
  val minutes = Signal {seconds.changedTo(0).count().apply() % 60}
  val hours = Signal {minutes.changedTo(0).count().apply() % 24}
  val days = hours.changedTo(0).count


  while (true) {
    Thread.sleep(0)
    println((seconds.now, minutes.now, hours.now, days.now))
    tick(())
  }

}



