import org.scalajs.dom
import rescala.engine.Engine
import rescala.graph.{CreationTicket, Struct}
import rescala.reactives.Signals.Diff
import rescala.reactives.{Observe, Signal}

import scala.language.higherKinds
import scalatags.JsDom.all._

package object rescalatags {

  implicit class SignalToScalatags[S <: Struct](signal: Signal[Tag, S]) {
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag(implicit ticket: CreationTicket[S], engine: Engine[S]): Frag = {
      ticket { implicit turn =>
        val result: Signal[dom.Node, S] = signal
          .map(_.render)
          .recover { case t => span(t.toString).render }
          .withDefault("".render)

        val observer = Observe.weak(result.change)(
          { case Diff(lastTag, newTag) =>
            if (lastTag.parentNode != null && !scalajs.js.isUndefined(lastTag.parentNode)) {
              lastTag.parentNode.replaceChild(newTag, lastTag)
            }
          },
          t => throw t)
        new REFrag(result, observer)
      }
    }

    class REFrag(rendered: Signal[dom.Node, S], val observe: Observe[S])(implicit engine: Engine[S]) extends Frag {
      def applyTo(t: dom.Element) = t.appendChild(rendered.now)
      def render: dom.Node = rendered.now
    }
  }


  implicit def attrValue[T: AttrValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Engine[S]): AttrValue[Sig[T]] = new AttrValue[Sig[T]] {
    def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def styleValue[T: StyleValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Engine[S]): StyleValue[Sig[T]] = new StyleValue[Sig[T]] {
    def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}
