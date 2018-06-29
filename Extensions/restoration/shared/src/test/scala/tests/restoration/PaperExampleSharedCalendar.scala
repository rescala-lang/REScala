package tests.restoration

import java.time.temporal.ChronoField
import java.time.{Instant => Date}

import io.circe.{Decoder, Encoder}
import org.scalatest.FreeSpec
import rescala.restoration.RestoringInterface
import cats.syntax.either._
import rescala.restoration.ReCirce._



class PaperExampleSharedCalendar extends FreeSpec {

  val interface = RestoringInterface()
  import interface._

  /* coders for serialization */
  implicit val instantWriter: Encoder[Date] = Encoder.encodeString.contramap[Date](_.toString)
  implicit val instantReader: Decoder[Date] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(java.time.Instant.parse(str)).leftMap(t => "Instant: " + t.getMessage)
  }

  implicit val taskDecoder: io.circe.Decoder[Entry]
  = io.circe.Decoder.forProduct3[String, String, List[String], Entry]("title", "date", "names") { (title, date, names) =>
    addNextNames(names: _*)
    Entry(Var(title), Var(java.time.Instant.parse(date)))
  }
  implicit val taskEncoder: io.circe.Encoder[Entry]
  = io.circe.Encoder.forProduct3[String, String, List[String], Entry]("decs", "done", "names") { t =>
    (t.title.readValueOnce, t.date.readValueOnce.toString, List(getName(t.title), getName(t.date)))
  }

  object Date { def today(): Date = java.time.Instant.now() }
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


  case class Entry(title: Signal[String], date: Signal[Date])


  "the paper shared calendar example" in {
    val newEntry = Evt[Entry]()
    val automaticEntries: Event[Entry] = App.nationalHolidays()
    val allEntries = newEntry || automaticEntries

    val selectedDay: Var[Date] = Var(Date.today)(recirce, implicitly)
    val selectedWeek = Signal {Week.of(selectedDay.value)}

    val entrySet: Signal[Set[Entry]] =
//      if (distribute) ReplicatedSet("SharedEntries").collect(allEntries)
//      else
      allEntries.fold(Set.empty[Entry]) { (entries, entry) => entries + entry }(implicitly, recirce[Set[Entry]])


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
  }

}
