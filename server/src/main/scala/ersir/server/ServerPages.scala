package ersir.server

import akka.http.scaladsl.model._
import io.circe.Encoder
import io.circe.syntax._
import scalatags.Text.attrs.{`for`, `type`, action, cls, content, href, id, rel, src, title, value, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{body, div, fieldset, form, frag, h1, head, html, input, label, legend, link, meta, script}
import scalatags.Text.tags2.section
import scalatags.text.Frag

class ServerPages() {

  val path_css: String = "css"
  val path_js: String = "js"


  def makeHtml(stuff: Frag*): Tag =
    html(
      head(
        title := "EmergenCity RSS Reader",
        link(href := path_css, rel := "stylesheet", `type` := MediaTypes.`text/css`.toString()),
//        script(`type` := "text/javascript", src := "https://unpkg.com/mqtt/dist/mqtt.min.js"),
        meta(attrname := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"))
    )(stuff: _*)

  def htmlResponse(tag: Tag): HttpResponse = HttpResponse(entity = HttpEntity(
    ContentType(MediaTypes.`text/html`, HttpCharsets.`UTF-8`),
    "<!DOCTYPE html>" + tag.render))

  val fullHtml: Tag = makeHtml(body("if nothing happens, your javascript does not work"), script(src := path_js))

  val landing: HttpResponse = htmlResponse(fullHtml)

  def jsonResponse[T: Encoder](value: T): HttpResponse = HttpResponse(entity = HttpEntity(
    ContentType(MediaTypes.`application/json`),
    value.asJson.noSpaces))

  def labelledInput(name: String, inputType: String = "text"): Frag =
    div(cls := "pure-control-group",
        label(name, `for` := name), input(id := name, `type` := inputType, attrname := name)
    )


  val toolsPage: Tag = makeHtml(
    body(h1("Tools"),
         makeToolForm("stop", Nil),
         makeToolForm("import", Seq("id", "name", "path")),
         makeToolForm("add", Seq("url"))
    )
  )

  private def makeToolForm(formAction: String, inputs: Seq[String]) = {
    section(
      fieldset(legend(formAction.capitalize),
               form(cls := "pure-form pure-form-aligned", action := formAction,
                    frag(inputs.map(labelledInput(_)): _*),
                    div(cls := "pure-controls",
                        input(`type` := "submit", cls := "pure-button", value := formAction)))))
  }
  val toolsResponse: HttpResponse = htmlResponse(toolsPage)

}
