import org.scalajs.dom
import org.scalajs.dom.{Element, html}
import rescala._

import scalatags.JsDom.all._
import scala.language.implicitConversions
import scalatags.generic
import scalatags.generic.Modifier

package object rescalatags {

  /**
    * Wraps reactive fragments in spans, so they can be referenced/replaced
    * when the Signal changes.
    */
  implicit def fragment[T](signal: Signal[T])(implicit f: T => Frag): Frag = {
    htmlTag(Signal {span(signal())})
  }

  /**
    * Sticks some Signal into a Scalatags fragment, which means hooking up an
    * changed event handler to propagate changes into the DOM via the element's
    * ID. Monkey-patches the handler onto the element itself so we have a
    * reference to kill it when the element leaves the DOM (e.g. gets deleted).
    */
  implicit def htmlTag(signal: Signal[HtmlTag]): HtmlTag = {
    new generic.TypedTag[dom.Element, html.Element, dom.Node] {

      val rendered = signal.map(_.render)

      override type Self = HtmlTag
      override def tag: String = signal.now.tag
      override def modifiers: List[Seq[Modifier[Element]]] = signal.now.modifiers
      override def apply(xs: Modifier[Element]*): Self = htmlTag(signal.map(_.apply(xs: _*)))


      rendered.change.observe { case (lastTag, newTag) =>
        if (lastTag.parentElement != null && !scalajs.js.isUndefined(lastTag.parentElement)) {
          lastTag.parentElement.replaceChild(newTag, lastTag)
        }
      }

      def applyTo(t: dom.Element) = t.appendChild(rendered.now)
      def render = rendered.now
    }
  }

  implicit def attrValue[T: AttrValue] = new AttrValue[Signal[T]] {
    def apply(t: dom.Element, a: Attr, signal: Signal[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def styleValue[T: StyleValue] = new StyleValue[Signal[T]] {
    def apply(t: dom.Element, s: Style, signal: Signal[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}
