package examples.circularmotion

import reactives.default._

object SignalVersion extends App {

  // Time and radius change over time
  val time   = Var(0)
  val radius = Signal { time.value % 10 } // The circle expands periodically

  // Constants in uniform motion
  val rotationPeriod  = 20
  val angularVelocity = 2 * 3.14 / rotationPeriod

  val speed        = Signal { angularVelocity * radius.value }
  val angle        = Signal { ((angularVelocity * time.value / 3.14) * 180) % 360 }
  val acceleration = Signal { angularVelocity * angularVelocity * radius.value }
  val space        = Signal { speed.value * time.value }

  // Print all the results.
  // Note that the order in which the items are printed is not deterministic.
  radius.changed observe { x => print(f"Radius: $x%d  ") }
  speed.changed observe { x => print(f"Speed: $x%.2f  ") }
  angle.changed observe { x => print(f"Angle: $x%.2f  ") }
  acceleration.changed observe { x => print(f"Acceleration: $x%.2f  ") }
  space.changed observe { x => println(f"Space: $x%.2f  ") }

  while (true) {
    Thread `sleep` 200
    time.transform(_ + 1)
  }

}
