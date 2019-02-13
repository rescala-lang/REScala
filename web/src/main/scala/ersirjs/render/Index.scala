package ersirjs.render

import ersirjs.{Actions, Icons}
import org.scalajs.dom.html
import rescala.default._
import rescala.Tags._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.{article, main}

class Index(actions: Actions, connected: Signal[String], list: Signal[List[String]]) {


  def gen(): JsDom.TypedTag[html.Body] = {

    val articles = list.map { itemsToDisplay =>
      SeqFrag(itemsToDisplay.map { str =>
        val split: Int => Option[String] = str.split("\n", 2).lift
        article(lang := "en",
                div(cls := "pic", style := "background-image: url(https://www.digitalstadt-darmstadt.de/wp-content/uploads/2019/02/telekom_com_Darmstadt_5G_3-1030x687.jpg);"),
                div(
                  h1(stringFrag(split(0).getOrElse(""))),
                  stringFrag(split(1).getOrElse(""))))
      })
    }
    body(id := "index",
         header(cls := connected, img(cls := "logo", src := "static/logo-small.svg"),
                Icons.lamp),
         main(articles.asModifier))


  }

}
