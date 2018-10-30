package live

import java.util.concurrent.ThreadLocalRandom

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{UIEvent, document}
import rescala.core.ReSerializable
import rescala.restoration.{LocalStorageStore, ReCirce}
import rescala.restoration.ReCirce.{recirce, varDecoder, varEncoder}
import rescala.debuggable.ChromeDebuggerInterface
import rescalatags._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.scalajs.js.annotation.JSExportTopLevel

object Demo {

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._

  implicit val taskDecoder: io.circe.Decoder[Task] = io.circe.Decoder.decodeTuple2[Var[String], Var[Boolean]].map { case (desc, done) =>
    new Task(desc, done)
  }
  implicit val taskEncoder: io.circe.Encoder[Task] = io.circe.Encoder.encodeTuple2[Var[String], Var[Boolean]].contramap{ t =>
    (t.desc, t.done)
  }

  class Task(val desc : Var[String], val done : Var[Boolean]) {
    val editing = Var(false)(ReSerializable.doNotSerialize, implicitly)
  }
  object Task {
    def apply(desc: String, done: Boolean) = {
      val rn = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
      val descV = Var(desc)(implicitly, rn.toString)
      val doneV = Var(done)(implicitly, rn.toString + "b")
      storingEngine.registerSource(descV)
      storingEngine.registerSource(doneV)
      new Task(descV, doneV)
    }
  }

  @JSExportTopLevel("live.Demo.main")
  def main(): Unit = {
    ChromeDebuggerInterface.setup(storingEngine)

    val content = storingEngine.transaction(){ tx =>

      val temperature = Evt[Double]

      val filtered = temperature.filter(_ < 100).filter(_ >= -40)

      val history: Signal[Seq[Double]] = filtered.last(50)

      val aggregation: Signal[Seq[Double] => Double] = Signal({ x: Seq[Double] => x.sum / x.size })

      val aggregated = Signal {
        aggregation.value(history.value)
      }


      div(
        `class` := "todoapp",
        header(
          `class` := "header",
          h1("Temperatures")
        ),

        section(
          `class` := "main",
          `style` := Signal {if (history.value.isEmpty) "display:hidden" else ""},
          Signal.dynamic {
            ul(`class` := "todo-list",
               history().map { temp =>
                 li(`class` := "no-editing ",
                    div(
                      `class` := "view",
                      label(temp),
                      ),
                    )
               })
          }.asFrag
        ),

        div(
          `class` := "footer",
          `style` := Signal {if (history.value.isEmpty) "display:none" else ""},

          label("aggregated: "),
          Signal {span(aggregated.value)}.asFrag
        )
      )
    }

    document.body.replaceChild(content.render, document.body.firstElementChild)
  }
}
