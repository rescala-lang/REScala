import org.scalajs.dom
import rescala._

import scala.language.{higherKinds, implicitConversions}
import scalatags.JsDom.all._

package object rescalatags {

  implicit class SignalToScalatags(signal: Signal[Tag]) {
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag: Frag = {
      new Frag {
        val rendered: Signal[dom.Node] = signal
          .map(_.render)
          .recover(t => span(t.toString).render)
          .withDefault(() => "".render)

        rendered.change.observe { case (lastTag, newTag) =>
          if (lastTag.parentNode != null && !scalajs.js.isUndefined(lastTag.parentNode)) {
            lastTag.parentNode.replaceChild(newTag, lastTag)
          }
        }

        def applyTo(t: dom.Element) = t.appendChild(rendered.now)
        def render: dom.Node = rendered.now
      }
    }
  }


  implicit def attrValue[T: AttrValue, Sig[T2] <: Signal[T2]]: AttrValue[Sig[T]] = new AttrValue[Sig[T]] {
    def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def styleValue[T: StyleValue, Sig[T2] <: Signal[T2]]: StyleValue[Sig[T]] = new StyleValue[Sig[T]] {
    def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}
