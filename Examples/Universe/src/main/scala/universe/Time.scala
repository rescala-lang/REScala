package universe

import rescala.reactives.Signals

import universe.Globals.engine._


class Time {
  val tick = Evt[Unit]()

  val hours = tick.iterate(0)(_ + 1)
  val day = hours map (_ / 24)
  val hour = hours map (_ % 24)
  val week = day map (_ / 7)
  val timestring = Signals.lift(week, day, hour) { (w, d, h) => s"Week: $w Day: $d  hour: $h" }
  val newWeek = week.changed
  override def toString: String = timestring.now
}
