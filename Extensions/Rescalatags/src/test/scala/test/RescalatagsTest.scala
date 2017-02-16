package test

import org.scalajs.dom.Node
import org.scalajs.dom.html.Span
import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import rescala.Engines
import rescalatags._

import scalatags.JsDom.all._

class RescalatagsTest extends FlatSpec with TableDrivenPropertyChecks {

  forAll(Table("engine", Engines.all: _*)) { engine =>
    import engine._

    behavior of engine.toString

    it should s"put var into dom" in {
      val v = Var.empty[Tag]
      val frag: Node = v.asFrag.render
      assert(frag.textContent === "", "empty var gives empty frag")

      val outer = div(v.asFrag)
      val outerR = outer.render

      assert(outerR.innerHTML === "", "empty var into dom is empty")

      v.set(span("hallo welt"))
      assert(outerR.innerHTML === "<span>hallo welt</span>", "setting var changes rendered outer tag")

      v.set(div("hallo div"))
      assert(outerR.innerHTML === "<div>hallo div</div>", "resetting var changes rendered outer tag")

    }

    it should s"put style into dom" in {
      val v = Var.empty[String]

      // we do this because this tends to be globally set from other tests …
      span.render.style.setProperty("backgroundColor", "green")

      val ourTag: Span = span(backgroundColor := v).render

      assert(ourTag.style.getPropertyValue("backgroundColor") === "green", "empty color does not render")

      v.set("red")
      assert(ourTag.style.getPropertyValue("backgroundColor") === "red", "changing var changes color")

      v.set("blue")
      assert(ourTag.style.getPropertyValue("backgroundColor") === "blue", "changing var changes color again")
    }

    it should s"put attribute into dom" in {
      val v = Var.empty[String]

      val ourTag = a(href := v).render

      assert(ourTag.outerHTML === a.render.outerHTML, "empty href does not render")

      v.set("www.rescala-lang.com")
      assert(ourTag.outerHTML === a(href := "www.rescala-lang.com").render.outerHTML, "changing var changes href")

      v.set("index.html")
      assert(ourTag.outerHTML === a(href := "index.html").render.outerHTML, "changing var changes href again")

    }
  }

}
