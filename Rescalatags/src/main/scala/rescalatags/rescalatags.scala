import org.scalajs.dom
import rescala._

import scala.language.implicitConversions
import scalatags.JsDom.all._

package object rescalatags {

  implicit class SignalToScalatags(signal: Signal[Tag]) {
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    implicit def asFragment: Frag = {
      new Frag {
        val rendered = signal.map(_.render)

        rendered.change.observe { case (lastTag, newTag) =>
          if (lastTag.parentNode != null && !scalajs.js.isUndefined(lastTag.parentNode)) {
            lastTag.parentNode.replaceChild(newTag, lastTag)
          }
        }

        def applyTo(t: dom.Element) = t.appendChild(rendered.now)
        def render = rendered.now
      }
    }
  }



  implicit def attrValue[T: AttrValue]: AttrValue[Signal[T]] = new AttrValue[Signal[T]] {
    def apply(t: dom.Element, a: Attr, signal: Signal[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def styleValue[T: StyleValue]: StyleValue[Signal[T]] = new StyleValue[Signal[T]] {
    def apply(t: dom.Element, s: Style, signal: Signal[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}
