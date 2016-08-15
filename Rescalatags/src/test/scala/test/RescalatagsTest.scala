package test

import org.scalajs.dom.Node
import org.scalajs.dom.html.Span
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescalatags._

import scalatags.JsDom.all._

class RescalatagsTest extends org.scalatest.FunSuite {

  rescala.engines.Engines.all.foreach(runWithEngine(_))

  def runWithEngine[S <: Struct](implicit engine: Engine[S, Turn[S]]): Unit = {
    import engine._

    test(s"$engine: put var into dom") {
      val v = Var.empty[Tag]
      val frag: Node = SignalToScalatags[S](v).asFrag.render
      assert(frag.textContent === "", "empty var gives empty frag")

      val outer = div(v.asFrag)
      val outerR = outer.render

      assert(outerR.innerHTML === "", "empty var into dom is empty")

      v.set(span("hallo welt"))
      assert(outerR.innerHTML === "<span>hallo welt</span>", "setting var changes rendered outer tag")

      v.set(div("hallo div"))
      assert(outerR.innerHTML === "<div>hallo div</div>", "resetting var changes rendered outer tag")

    }

    test(s"$engine: put style into dom") {
      val v = Var.empty[String]

      span.render.style.setProperty("backgroundColor", "green")

      val ourTag: Span = span(backgroundColor := v).render

      assert(ourTag.style.getPropertyValue("backgroundColor") === "green", "empty color does not render")

      v.set("red")
      assert(ourTag.style.getPropertyValue("backgroundColor") === "red", "changing var changes color")

      v.set("blue")
      assert(ourTag.style.getPropertyValue("backgroundColor") === "blue", "changing var changes color again")
    }

    test(s"$engine: put attribute into dom") {
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
