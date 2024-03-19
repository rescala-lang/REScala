package copl

import copl.Codecs.given
import kofre.base.Lattice
import kofre.datatypes.contextual.ReplicatedList
import kofre.dotted.Dotted
import kofre.syntax.ReplicaId
import loci.registry.{Binding, Registry}
import loci.serializer.jsoniterScala.*
import org.scalajs.dom.html.{Div, Input, Paragraph}
import org.scalajs.dom.{HTMLElement, UIEvent, document, window}
import rescala.default
import rescala.default.*
import rescala.extra.Tags.*
import rescala.extra.distribution.Network
import dtn.CrdtConnector
import scalatags.JsDom.all.*
import scalatags.JsDom.{Attr, TypedTag}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global


case class Chatline(author: String, message: String)

case class Epoche[T](number: Int, payload: T)

@JSExportTopLevel("ChatApp")
object ChatApp {

//  val registry = new Registry

  given ReplicaId = ReplicaId.gen()

  given epocheInstance[T: Lattice]: Lattice[Epoche[T]] = new Lattice[Epoche[T]] {
    override def merge(left: Epoche[T], right: Epoche[T]): Epoche[T] =
      if (left.number > right.number) left
      else if (left.number < right.number) right
      else Epoche(number = left.number, payload = Lattice[T].merge(left.payload, right.payload))
  }

  @JSExport("start")
  def start(): Unit = {
    val contents = getContents()
    document.body.replaceChild(contents.render, document.body.firstChild)
    document.body.appendChild(p(style := "height: 3em").render)
//    document.body.appendChild(WebRTCHandling(registry).webrtcHandlingArea.render)
    document.body.appendChild(getConnectorContents())
  }

  def getConnectorContents(): HTMLElement = {
    val portInput = input(placeholder := "dtnd ws port").render
    val connectButton = button(onclick := {() => {
      CrdtConnector.connectToWS(portInput.value.toInt)
    }}).render
    connectButton.textContent = "Connect"

    div(portInput, connectButton).render
  }

  def getContents(): TypedTag[Div] = {

    val nameInput = input(placeholder := "Your Name")
    val (nameEvent, renderedName): (Event[String], Input) =
      RenderUtil.inputFieldHandler(nameInput, oninput, clear = false)

    val messageInput = input()
    val (messageEvent, renderedMessage): (Event[String], Input) =
      RenderUtil.inputFieldHandler(messageInput, onchange, clear = true)

    val nameSignal: Signal[String] = Storing.storedAs("name", "") { init =>
      renderedName.value = init
      nameEvent.hold(init)
    }

    val chatline: Event[Chatline] = messageEvent.map { msg => Chatline(nameSignal.value, msg) }

    val deleteAll = Evt[Unit]()

    val history: Signal[Epoche[Dotted[ReplicatedList[Chatline]]]] =
      Storing.storedAs("history", Epoche(0, Dotted(ReplicatedList.empty[Chatline]))) { initial =>
        Fold(initial)(
          chatline act { line => current merge current.copy(payload = current.payload.prepend(line)) },
          deleteAll act { arg =>
            current merge Epoche(number = current.number + 1, Dotted(ReplicatedList.empty[Chatline]))
          }
        )
      }

    //Network.replicate(history, registry)(Binding("history"))
    CrdtConnector.connectGraph(history)

    val chatDisplay = Signal.dynamic {
      val reversedHistory = history.value.payload.toList.reverse
      reversedHistory.map { case Chatline(author, message) =>
        val styleString = if (chatline.value.contains(Chatline(author, message))) "color: red" else ""
        p(s"${author}: $message", style := styleString)
      }
    }

    div(div(chatDisplay.asModifierL), renderedName, renderedMessage)
  }

}
