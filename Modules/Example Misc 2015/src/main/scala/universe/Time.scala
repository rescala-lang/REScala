package universe

import reactives.default.*

class Time {
  val tick = Evt[Unit]()

  val hours                     = tick.count()
  val day                       = hours map (_ / 24)
  val hour                      = hours map (_ % 24)
  val week                      = day map (_ / 7)
  val timestring                = Signal(s"Week: ${week.value} Day: ${day.value}  hour: ${hour.value}")
  val newWeek                   = week.changed
  override def toString: String = timestring.readValueOnce
}
