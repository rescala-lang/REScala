package examples.meanwindow

import reactives.default._

object EventWindows extends App {

  val e = Evt[Double]()
  // val all = e.list()
  val window = e.list(5)
  val mean = Signal {
    window.value.sum /
    window.value.length
  }
  mean.changed observe { println(_) }

  e.fire(2)
  e.fire(1)
  e.fire(3)
  e.fire(4)
  e.fire(1)
  e.fire(1)
}
