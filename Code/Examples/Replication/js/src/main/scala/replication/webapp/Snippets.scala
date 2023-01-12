package replication.webapp

import org.scalajs.dom
import org.scalajs.dom.html.Element
import org.scalajs.dom.{MouseEvent, document}
import replication.webapp.MetaInfo
import rescala.default.*
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{HtmlTag, Modifier, alt, bindJsAnyLike, button, onclick, stringFrag, *}
import scalatags.JsDom.tags2.{nav, section}

import scala.scalajs.js.URIUtils.encodeURIComponent
object Snippets {

  def lcButton(action: => Unit, m: Modifier*): HtmlTag =
    button(onclick := { (e: MouseEvent) =>
      if (e.button == 0) {
        e.preventDefault()
        action
      }
    })(m: _*)

  def meta(meta: MetaInfo): Signal[TypedTag[Element]] = {
    val connectionStatus = Signal {
      meta.connection.value match {
        case 0     => stringFrag(s"disconnected (attempt № ${meta.reconnecting.value})")
        case other => stringFrag(s"$other active")
      }
    }
    Signal {
      section(List[Frag](
        s"connection status: ",
        connectionStatus.value,
        br()
      )).asInstanceOf[HtmlTag]
    }
  }
}
