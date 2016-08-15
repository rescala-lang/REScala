package tests

import org.scalajs.dom.Node
import _root_.rescala._
import rescalatags._

import scalatags.JsDom.all._

class SimpleTest extends org.scalatest.FunSuite {
  test("put var into dom") {
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

  test("put style into dom") {
    val v = Var.empty[String]

    val ourTag = span(color := v).render

    assert(ourTag.style.getPropertyValue("color") === null, "empty color does not render")

    v.set("red")
    assert(ourTag.style.getPropertyValue("color") === "red", "changing var changes color")

    v.set("blue")
    assert(ourTag.style.getPropertyValue("color") === "blue", "changing var changes color again")
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
}
