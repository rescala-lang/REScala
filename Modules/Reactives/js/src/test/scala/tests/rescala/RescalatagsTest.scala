package tests.rescala

import org.scalajs.dom
import org.scalajs.dom.html.Span
import org.scalajs.dom.{Element, Node}
import reactives.extra.Tags
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.*
import scalatags.generic.StylePair
import tests.rescala.testtools.RETests

import reactives.default.*
import reactives.extra.Tags.*

import scala.util.chaining.scalaUtilChainingOps

class RescalatagsTest extends RETests {

  test("put var into dom") {

    val v        = Var.empty[Element]
    val rendered = div().render.reattach(v)
    assertEquals(rendered.textContent, "", "empty var gives empty frag")

    assertEquals(rendered.innerHTML, "", "empty var into dom is empty")

    v.set(span("hallo welt").render)
    assertEquals(rendered.innerHTML, "<span>hallo welt</span>", "setting var changes rendered outer tag")

    v.set(div("hallo div").render)
    assertEquals(rendered.innerHTML, "<div>hallo div</div>", "resetting var changes rendered outer tag")

  }

  given RangeSplice[Modifier] with {
    override def splice(anchor: dom.Element, range: dom.Range, value: Modifier): Unit =
      val parent = range.commonAncestorContainer
      parent match
        case elem: dom.Element => value.applyTo(elem)
  }

  test("put style into dom") {
    val v: Var[String] = Var.empty[String]

    val ourTag: Span = span.render.reattach(v.map(backgroundColor := _))

    assertEquals(ourTag.style.getPropertyValue(backgroundColor.cssName), "", "empty color does not render")

    v.set("red")
    assertEquals(ourTag.style.getPropertyValue(backgroundColor.cssName), "red", "changing var changes color")

    v.set("blue")
    assertEquals(ourTag.style.getPropertyValue(backgroundColor.cssName), "blue", "changing var changes color again")
  }

  test("put attribute into dom") {
    val v = Var.empty[String]

    val ourTag = a().render.reattach(v.map(href := _))

    assertEquals(ourTag.outerHTML, a.render.outerHTML, "empty href does not render")

    v.set("www.rescala-lang.com")
    assertEquals(ourTag.outerHTML, a(href := "www.rescala-lang.com").render.outerHTML, "changing var changes href")

    v.set("index.html")
    assertEquals(ourTag.outerHTML, a(href := "index.html").render.outerHTML, "changing var changes href again")

  }

  test("work with multiple childern") {

    val v     = Var(Seq(span("hey"), span("ho")))
    def vrend = v.map(_.map(_.render))

    val outerR                 = div().render.reattach(vrend)
    val outerWithOtherChildren = div(span("before")).render.reattach(vrend).tap(_.append(span("after").render))

    assertEquals(outerR.innerHTML, "<span>hey</span><span>ho</span>", "render fragments")
    assertEquals(
      "<span>before</span><span>hey</span><span>ho</span><span>after</span>",
      outerWithOtherChildren.innerHTML,
      "render fragments2"
    )

    v.set(Seq(span("hallo welt")))
    assertEquals(outerR.innerHTML, "<span>hallo welt</span>", "setting to less elements works")
    assertEquals(
      "<span>before</span><span>hallo welt</span><span>after</span>",
      outerWithOtherChildren.innerHTML,
      "setting to less elements works2"
    )

    v.set(Seq(span("hey2"), span("ho2")))
    assertEquals(outerR.innerHTML, "<span>hey2</span><span>ho2</span>", "increasing works")
    assertEquals(
      "<span>before</span><span>hey2</span><span>ho2</span><span>after</span>",
      outerWithOtherChildren.innerHTML,
      "increasing works2"
    )

  }
}
