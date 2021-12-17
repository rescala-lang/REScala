package ersir

import akka.http.scaladsl.model._
import scalatags.Text.Frag
import scalatags.Text.attrs.{`type`, attr, content, href, rel, src, title, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{SeqFrag, body, frag, head, html, link, meta, script}

class ServerPages() {

  def makeHtml(stuff: Frag*): Tag =
    html(
      head(
        title := "EmergenCity RSS Reader",
        link(rel := "manifest", href := "static/manifest.json"),
        link(rel := "icon", href     := "static/icon.png", attr("sizes") := "192x192"),
        SeqFrag(ResourcePaths.css.map { css =>
          link(href := css, rel := "stylesheet", `type` := MediaTypes.`text/css`.toString())
        }),
        meta(attrname := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
        //      script(raw("""if('serviceWorker' in navigator) {
        // navigator.serviceWorker
        //         .register('serviceworker.js')
        //         .then(function() { console.log('Service Worker Registered'); }); }"""))
      )
    )(stuff: _*)

  def htmlResponse(tag: Tag): HttpResponse =
    HttpResponse(entity =
      HttpEntity(
        ContentType(MediaTypes.`text/html`, HttpCharsets.`UTF-8`),
        "<!DOCTYPE html>" + tag.render
      )
    )

  val fullHtml: Tag = makeHtml(
    body("if nothing happens, your javascript does not work"),
    frag(ResourcePaths.js.map(js => script(src := js)))
  )

  val landing: HttpResponse = htmlResponse(fullHtml)

}
