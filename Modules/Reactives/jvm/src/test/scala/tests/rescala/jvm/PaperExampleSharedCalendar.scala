package tests.rescala.jvm

import reactives.default.*

import java.time.temporal.ChronoField
import java.time.{Clock, LocalDate as Date}

class PaperExampleSharedCalendar extends munit.FunSuite {

  object Date { def today(): Date = java.time.LocalDate.now(Clock.systemUTC()) }
  object Week {

    /** use beginning of week as representation for current week */
    def of(date: Date): Date = date.`with`(ChronoField.DAY_OF_WEEK, 1)
  }
  object App {
    val holiday: Evt[Entry]              = Evt[Entry]()
    def nationalHolidays(): Event[Entry] = holiday
  }
  object Log { def appendEntry(entry: Entry): Unit = println(s"Log: $entry") }
  object Ui {
    def displayEntryList(entry: Set[Entry]): Unit = println(s"UI: $entry")
    def displayError(error: Throwable): Unit      = println(s"Error: $error")
  }

  object DisconnectedSignal extends Throwable

  case class Entry(title: Signal[String], date: Signal[Date]) {
    override def toString: String = s"Entry(${title.readValueOnce}, ${date.readValueOnce})"
  }

  test("the paper shared calendar example") {
    val newEntry                       = Evt[Entry]()
    val automaticEntries: Event[Entry] = App.nationalHolidays()
    val allEntries                     = newEntry || automaticEntries

    val selectedDay: Var[Date] = Var(Date.today())
    val selectedWeek           = Signal { Week.of(selectedDay.value) }

    val entrySet: Signal[Set[Entry]] =
      allEntries.fold(Set.empty[Entry]) { (entries, entry) => entries + entry }

    val selectedEntries = Signal.dynamic {
      entrySet.value.filter { entry =>
        try selectedWeek.value == Week.of(entry.date.value)
        catch { case DisconnectedSignal => false }
      }
    }

    allEntries.observe(Log.appendEntry)
    selectedEntries.observe(
      onValue = Ui.displayEntryList,
      onError = Ui.displayError
    )

    newEntry.fire(Entry(Var("Presentation"), Var(Date.today())))
    newEntry.fire(Entry(Var("Prepare Presentation"), Var(Date.today().minusDays(7))))
    selectedDay.set(Date.today().minusDays(7))
  }

}
