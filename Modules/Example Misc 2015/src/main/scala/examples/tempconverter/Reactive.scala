package examples.tempconverter

// Wrap a bit of Swing, as if it were an FRP library

// Escala lib + behaviour extensions

import rescala.default._

// Scala swing events
import scala.swing._
import scala.swing.event._

// could we actually use Reactive[Any] and use toString?
trait ReactiveText extends Reactor {
  protected lazy val userSet = Var(text)

  def text: String
  def text_=(s: String): Unit
  // in signal
  def text_=(value: Signal[String]): Unit = {
    this.text_=(value.now)
    value.changed observe { (t: String) => this.text_=(t) }
    ()
  }
  // out signal
  lazy val text_out = Signal { userSet.value }
  reactions += { case EditDone(_) => userSet set text }
}

class ReactiveTextfield extends TextField with ReactiveText {
  listenTo(this)
}
class ReactiveLabel  extends Label with ReactiveText
class ReactiveButton extends Button with ReactiveText {
  // wrap the event to escala
  val clicked = Evt[ButtonClicked]()
  reactions += { case c @ ButtonClicked(_) => clicked.fire(c) }
}
