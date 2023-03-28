package tests.rescala

import org.scalajs.dom.{Element, Node}
import org.scalajs.dom.html.Span
import rescala.extra.Tags
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import tests.rescala.testtools.RETests

class RescalatagsTest extends RETests {
  multiEngined { engine =>
    val te = new Tags(engine, addDebuggingIds = false)
    import te._
    import te.api._

    test("put var into dom") {
      val v              = Var.empty[TypedTag[Element]]
      val rendered: Node = div(v.asModifier).render
      assert(rendered.textContent === "", "empty var gives empty frag")

      val outer  = div(v.asModifier)
      val outerR = outer.render

      assert(outerR.innerHTML === "", "empty var into dom is empty")

      v.set(span("hallo welt"))
      assert(outerR.innerHTML === "<span>hallo welt</span>", "setting var changes rendered outer tag")

      v.set(div("hallo div"))
      assert(outerR.innerHTML === "<div>hallo div</div>", "resetting var changes rendered outer tag")

    }

    test("put style into dom") {
      val v: Var[String] = Var.empty[String]

      val ourTag: Span = span(backgroundColor := v).render

      assert(ourTag.style.getPropertyValue(backgroundColor.cssName) === "", "empty color does not render")

      v.set("red")
      assert(ourTag.style.getPropertyValue(backgroundColor.cssName) === "red", "changing var changes color")

      v.set("blue")
      assert(ourTag.style.getPropertyValue(backgroundColor.cssName) === "blue", "changing var changes color again")
    }

    test("put attribute into dom") {
      val v = Var.empty[String]

      val ourTag = a(href := v).render

      assert(ourTag.outerHTML === a.render.outerHTML, "empty href does not render")

      v.set("www.rescala-lang.com")
      assert(ourTag.outerHTML === a(href := "www.rescala-lang.com").render.outerHTML, "changing var changes href")

      v.set("index.html")
      assert(ourTag.outerHTML === a(href := "index.html").render.outerHTML, "changing var changes href again")

    }

    test("work with multiple childern") {

      val v = Var(Seq(span("hey"), span("ho")))

      val outer                  = div(v.asModifierL)
      val outerR                 = outer.render
      val outerWithOtherChildren = div(span("before"), v.asModifierL, span("after"))
      val oR                     = outerWithOtherChildren.render

      assert(outerR.innerHTML === "<span>hey</span><span>ho</span>", "render fragments")
      assert(
        oR.innerHTML === "<span>before</span><span>hey</span><span>ho</span><span>after</span>",
        "render fragments2"
      )

      v.set(Seq(span("hallo welt")))
      assert(outerR.innerHTML === "<span>hallo welt</span>", "setting to less elements works")
      assert(
        oR.innerHTML === "<span>before</span><span>hallo welt</span><span>after</span>",
        "setting to less elements works2"
      )

      v.set(Seq(span("hey2"), span("ho2")))
      assert(outerR.innerHTML === "<span>hey2</span><span>ho2</span>", "increasing works")
      assert(
        oR.innerHTML === "<span>before</span><span>hey2</span><span>ho2</span><span>after</span>",
        "increasing works2"
      )

    }
  }

}
