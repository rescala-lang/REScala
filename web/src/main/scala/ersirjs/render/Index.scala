package ersirjs.render

import ersir.shared.Emergentcy
import ersirjs.{Actions, Icons}
import org.scalajs.dom.html
import rescala.default._
import rescala.Tags._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.{article, main}

class Index(actions: Actions, connected: Signal[String], list: Signal[List[Emergentcy]]) {


  def gen(): JsDom.TypedTag[html.Body] = {

    val articles = list.map { itemsToDisplay =>
      SeqFrag(itemsToDisplay.map { emergentcy =>
        article(lang := "de",
                div(cls := "pic", style := s"background-image: url(${emergentcy.img});"),
                div(
                  h1(stringFrag(emergentcy.title)),
                  stringFrag(emergentcy.desc)))
      })
    }
    body(id := "index",
         header(cls := connected, img(cls := "logo", src := "static/logo-small.svg"),
                Icons.lamp),
         main(articles.asModifier))


  }

}
