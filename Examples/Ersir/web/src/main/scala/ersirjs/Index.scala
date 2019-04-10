package ersirjs

import ersir.shared.Posting
import ersirjs.ErsirJS.Postings
import org.scalajs.dom.{CanvasRenderingContext2D, FileReader, UIEvent, html}
import rescala.Tags._
import rescala.default._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.{article, main}

class Index(connected: Signal[String]) {

  val textinput  = textarea.render
  val imageinput = input(`type` := "file", accept := "image/*", attr("capture") := "camera",
                         "Take a picture").render

  val addPost = Events.fromCallback[Posting] { postCB =>
    onclick := { _: UIEvent =>
      if (scala.scalajs.js.isUndefined(imageinput.files) || scala.scalajs.js.isUndefined(
        imageinput.files(0))) {
        val posting = Posting.parse(textinput.value.toString)
        textinput.value = ""
        postCB(posting)
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
            val posting = Posting.parse(textinput.value.toString, encoded)
            textinput.value = ""
            imageinput.value = imageinput.defaultValue
            postCB(posting)
          }

          imageTag.src = dataUrl

        }
        reader.readAsDataURL(imageinput.files(0))
      }
    }
  }

  val reset = Events.fromCallback[UIEvent] {onclick := _}


  def gen(list: Signal[Postings]): JsDom.TypedTag[html.Body] = {

    val articles = list.map { itemsToDisplay =>
      itemsToDisplay.value.value.map { emergentcy =>
        article(lang := "de",
                if (emergentcy.img.isEmpty) frag() else div(cls := "pic",
                                                            style := s"background-image: url(${emergentcy.img});"),
                div(
                  h1(stringFrag(emergentcy.title)),
                  stringFrag(emergentcy.desc)))
      }
    }

    body(id := "index",
         header(cls := connected,
                Icons.disconnected,
                img(cls := "logo", src := "static/logo-small.svg"),
                Icons.lamp),
         article(cls := "controls",
                 textinput,
                 imageinput,
                 button("Post", addPost.value)),
         main(articles.asModifierL),
         article(cls := "controls",
                 button("Reset", reset.value),
                 button("Fullscreen",
                        onclick := {(_: UIEvent) => Fullscreen.toggleFullscreen()}))
    )


  }

}
