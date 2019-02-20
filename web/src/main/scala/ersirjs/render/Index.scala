package ersirjs.render

import ersir.shared.Emergentcy
import ersirjs.{Actions, ErsirPost, Icons}
import org.scalajs.dom.{UIEvent, html}
import rescala.Tags._
import rescala.default._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.{article, main}

class Index(actions: Actions, connected: Signal[String], list: Signal[List[Emergentcy]]) {


  def gen(): JsDom.TypedTag[html.Body] = {

    val articles = list.map { itemsToDisplay =>
      SeqFrag(itemsToDisplay.map { emergentcy =>
        article(lang := "de",
                if (emergentcy.img.isEmpty) frag() else div(cls := "pic", style := s"background-image: url(${emergentcy.img});"),
                div(
                  h1(stringFrag(emergentcy.title)),
                  stringFrag(emergentcy.desc)))
      })
    }
    val textinput = input.render
    body(id := "index",
         header(cls := connected, img(cls := "logo", src := "static/logo-small.svg"),
                Icons.lamp),
        article(textinput, button("Add", onclick := {(e: UIEvent) => ErsirPost.add(textinput.value.toString)})),
         main(articles.asModifier))


  }

}
