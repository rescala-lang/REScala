package examples.meanwindow

import rescala.default._

object EventWindows extends App {

  val e = Evt[Double]()
  // val all = e.list()
  val window = e.last(5)
  val mean = Signal {
    window().sum /
    window().length
  }
  mean.changed += { println(_) }

  e.fire(2)
  e.fire(1)
  e.fire(3)
  e.fire(4)
  e.fire(1)
  e.fire(1)
}
