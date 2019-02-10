package ersirjs.render

import ersirjs.Definitions.link_tools
import ersirjs.{Actions, Make, SearchUtil}
import org.scalajs.dom.html
import rescala.default._
import rescala.rescalatags._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.implicits.stringFrag

import scala.scalajs.js

class Index(actions: Actions, list: Signal[List[String]]) {


  def gen(): Signal[JsDom.TypedTag[html.Body]] = {

    rescala.reactives.Signals.lift(list) { itemsToDisplay =>

      val inputQuery = Var("")
      val inputField = input(value := inputQuery, `type` := "text", tabindex := "1", onkeyup := ({ (inp: html.Input) =>
        inputQuery.set(inp.value.toString.toLowerCase)
      }: js.ThisFunction))

      val filteredList = inputQuery.map { query =>
        if (query.isEmpty) itemsToDisplay
        else SearchUtil.search(query, itemsToDisplay.map(n => n -> n))
      }

      val firstSelected: Signal[Option[String]] = Signal {
        filteredList().headOption
      }

      val callback: Signal[() => Boolean] = firstSelected map { sel =>
        () => {
          false
        }
      }

      val searchForm = form(cls := "pure-form")(inputField, onsubmit := callback)

      body(id := "index",
        Make.navigation(Make.fullscreenToggle("fullscreen"), searchForm, link_tools("tools")),
           fieldset(filteredList.map(is => ul(is.map(li(_)))).asFrag))
    }
  }

}
