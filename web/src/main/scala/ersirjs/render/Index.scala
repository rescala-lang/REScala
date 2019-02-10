package ersirjs.render

import ersirjs.{Actions, SearchUtil}
import org.scalajs.dom.html
import rescala.default._
import rescala.rescalatags._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.implicits.stringFrag
import scalatags.JsDom.tags2.{article, main}

class Index(actions: Actions, list: Signal[List[String]]) {


  def gen(): Signal[JsDom.TypedTag[html.Body]] = {

    rescala.reactives.Signals.lift(list) { itemsToDisplay =>

      val fireSearch = Evt[html.Input]()
      val inputQuery = fireSearch.map(_.value.toLowerCase).latest("")

      val searchInputField = input(value := inputQuery,
                                   `type` := "text",
                                   id := "search",
                                   tabindex := "1",
                                   onkeyup := fireSearch)



      val filteredList = inputQuery.map { query =>
        if (query.isEmpty) itemsToDisplay
        else SearchUtil.search(query, itemsToDisplay.map(n => n -> n))
      }

      val searchForm = form(cls := "pure-form pure-form-aligned")(
        fieldset(`class` := "pure-control-group",
                 label(`for` := "search", "Search"),
                 searchInputField))

      val addForm = form(cls := "pure-form pure-form-aligned")(
        fieldset(`class` := "pure-control-group",
                 label(`for` := "add", "Add Entry"),
                 input(`type` := "text",
                       id := "add",
                       tabindex := "2"),
                 button(cls := "pure-button", "Post")))

      body(id := "index",
           img(src := "static/logo-small.svg"),
           searchForm,
           addForm,
           filteredList.map(is => main(is.map(article(_)))).asFrag)
    }
  }

}
