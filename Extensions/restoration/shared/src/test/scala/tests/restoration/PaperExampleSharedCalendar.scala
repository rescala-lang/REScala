package tests.restoration

import java.time.temporal.ChronoField
import java.time.{LocalDate => Date}

import cats.syntax.either._
import io.circe.{Decoder, Encoder}
import org.scalatest.FreeSpec
import rescala.restoration.ReCirce._
import rescala.restoration.RestoringInterface



class PaperExampleSharedCalendar extends FreeSpec {

  val interface = RestoringInterface()
  import interface._

  /* coders for serialization */
  implicit val instantWriter: Encoder[Date] = Encoder.encodeString.contramap[Date](_.toString)
  implicit val instantReader: Decoder[Date] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(java.time.LocalDate.parse(str)).leftMap(t => "Instant: " + t.getMessage)
  }

  implicit val entryDecoder: io.circe.Decoder[Entry]
  = io.circe.Decoder.forProduct3[String, String, List[String], Entry]("title", "date", "names") { (title, date, names) =>
    addNextNames(names: _*)
    Entry(Var(title), Var(java.time.LocalDate.parse(date)))
  }
  implicit val entryEncoder: io.circe.Encoder[Entry]
  = io.circe.Encoder.forProduct3[String, String, List[String], Entry]("decs", "done", "names") { t =>
    (t.title.readValueOnce, t.date.readValueOnce.toString, List(getName(t.title), getName(t.date)))
  }

  object Date { def today(): Date = java.time.LocalDate.now() }
  object Week {
    /** use beginning of week as representation for current week */
    def of(date: Date): Date = date.`with`(ChronoField.DAY_OF_WEEK, 1)
  }
  object App {
    val holiday: interface.Evt[Entry] = Evt[Entry]
    def nationalHolidays(): Event[Entry] = holiday
  }
  object Log { def appendEntry(entry: Entry): Unit = println(s"Log: $entry") }
  object Ui {
    def displayEntryList(entry: Set[Entry]): Unit = println(s"UI: $entry")
    def displayError(error: Throwable): Unit = println(s"Error: $error")
  }

  object DisconnectedSignal extends Throwable


  case class Entry(title: Signal[String], date: Signal[Date]) {
    override def toString: String = s"Entry(${title.readValueOnce}, ${date.readValueOnce})"
  }


  "the paper shared calendar example" in {
    val newEntry = Evt[Entry]()
    val automaticEntries: Event[Entry] = App.nationalHolidays()
    val allEntries = newEntry || automaticEntries

    val selectedDay: Var[Date] = Var(Date.today)
    val selectedWeek = Signal {Week.of(selectedDay.value)}

    val entrySet: Signal[Set[Entry]] =
//      if (distribute) ReplicatedSet("SharedEntries").collect(allEntries)
//      else
      allEntries.fold(Set.empty[Entry]) { (entries, entry) => entries + entry }

    val selectedEntries = Signal.dynamic {
      entrySet.value.filter { entry =>
        try selectedWeek.value == Week.of(entry.date.value)
        catch {case DisconnectedSignal => false}
      }
    }

    allEntries.observe(Log.appendEntry)
    selectedEntries.observe(
      onValue = Ui.displayEntryList,
      onError = Ui.displayError)


    newEntry.fire(Entry(Var("Presentation"), Var(Date.today())))
    newEntry.fire(Entry(Var("Prepare Presentation"), Var(Date.today().minusDays(7))))
    selectedDay.set(Date.today().minusDays(7))
  }


}
