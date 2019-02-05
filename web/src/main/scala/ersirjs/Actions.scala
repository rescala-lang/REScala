package ersirjs

import ersirjs.AppState.IndexState
import ersirjs.Definitions.class_button
import org.scalajs.dom.MouseEvent
import rescala.default._
import scalatags.JsDom.all.{HtmlTag, Modifier, Tag, bindJsAnyLike, button, onclick}

class Actions(manualStates: Evt[AppState]) {

  private def onLeftClick(a: => Unit): Modifier = onclick := { (e: MouseEvent) =>
    if (e.button == 0) {
      e.preventDefault()
      a
    }
  }

  private def gotoIndex(): Unit = manualStates.fire(IndexState)




  def button_index(ts: Modifier*): Tag = lcButton(gotoIndex(), ts: _*)


  def lcButton(action: => Unit, m: Modifier*): HtmlTag = button(class_button, onLeftClick(action))(m: _*)
}
