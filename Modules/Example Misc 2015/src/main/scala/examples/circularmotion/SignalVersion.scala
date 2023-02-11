package examples.circularmotion

import rescala.default._

object SignalVersion extends App {

  // Time and radius change over time
  val time   = Var(0)
  val radius = Signal { time() % 10 } // The circle expands periodically

  // Constants in uniform motion
  val rotationPeriod  = 20
  val angularVelocity = 2 * 3.14 / rotationPeriod

  val speed        = Signal { angularVelocity * radius() }
  val angle        = Signal { ((angularVelocity * time() / 3.14) * 180) % 360 }
  val acceleration = Signal { angularVelocity * angularVelocity * radius() }
  val space        = Signal { speed() * time() }

  // Print all the results.
  // Note that the order in which the items are printed is not deterministic.
  radius.changed += { x => print(f"Radius: $x%d  ") }
  speed.changed += { x => print(f"Speed: $x%.2f  ") }
  angle.changed += { x => print(f"Angle: $x%.2f  ") }
  acceleration.changed += { x => print(f"Acceleration: $x%.2f  ") }
  space.changed += { x => println(f"Space: $x%.2f  ") }

  while (true) {
    Thread sleep 200
    time.transform(_ + 1)
  }

}
