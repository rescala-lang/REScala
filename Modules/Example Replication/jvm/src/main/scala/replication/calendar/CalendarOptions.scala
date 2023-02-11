package replication.calendar

import de.rmgk.options.*

case class CalendarOptions(
    id: Argument[String, Single, Style.Named] = Argument(),
    listenPort: Argument[Int, Single, Style.Named] = Argument(),
    connectTo: Argument[String, List, Style.Named] = Argument(_.valueName("<ip:port>"), default = Some(Nil))
)
