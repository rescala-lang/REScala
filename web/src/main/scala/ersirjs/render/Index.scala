package ersirjs.render

import ersir.shared.Emergentcy
import ersirjs.{Actions, ErsirPost, Icons}
import org.scalajs.dom.{CanvasRenderingContext2D, FileReader, UIEvent, html}
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
    val textinput = textarea.render
    val imageinput = input(`type` :="file", accept :="image/*", attr("capture") := "camera",
                           "Take a picture").render
    body(id := "index",
           header(cls := connected, img(cls := "logo", src := "static/logo-small.svg"),
                Icons.lamp),
        article(cls := "addentry",
                textinput,
                div("Add an Image: ", imageinput),
                button("Post", onclick := { _: UIEvent =>
          if (scala.scalajs.js.isUndefined(imageinput.files) || scala.scalajs.js.isUndefined(imageinput.files(0))) {
            ErsirPost.add(textinput.value.toString)
            textinput.value = ""
          }
          else {
            val reader = new FileReader()
            println(s"reading ${imageinput.files(0)}")
            reader.onload = { _ =>
              val dataUrl = reader.result.asInstanceOf[String]
              println(s"loaded $dataUrl")

              val imageTag = img.render


              imageTag.onload = { _ =>

                val canvasTag = canvas.render
                val ctx = canvasTag.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

                val width = 300
                val height = Math.floor(width.toDouble * imageTag.height / imageTag.width).toInt
                canvasTag.width = width
                canvasTag.height = height

                ctx.drawImage(imageTag, 0, 0, width.toDouble, height.toDouble)

                val encoded = canvasTag.toDataURL("image/jpeg", 0.5)
                println(s"printing $encoded")
                ErsirPost.add(textinput.value.toString, encoded)
              }

              imageTag.src = dataUrl

            }
            reader.readAsDataURL(imageinput.files(0))
          }
        })),
         main(articles.asModifier))


  }

}
