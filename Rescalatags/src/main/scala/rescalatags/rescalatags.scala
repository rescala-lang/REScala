import org.scalajs.dom
import rescala.engines.{Engine, Ticket}
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.Signal

import scala.language.{higherKinds, implicitConversions}
import scalatags.JsDom.all._

package object rescalatags {

  implicit class SignalToScalatags[S <: Struct](signal: Signal[Tag, S]) {
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag(implicit ticket: Ticket[S], engine: Engine[S, Turn[S]]): Frag = {
      val rendered: Signal[dom.Node, S] = ticket { implicit turn =>

        val result: Signal[dom.Node, S] = signal
          .map(_.render)
          .recover(t => span(t.toString).render)
          .withDefault("".render)

        result.change.observe { diff =>
          val (lastTag, newTag) = diff.pair
          if (lastTag.parentNode != null && !scalajs.js.isUndefined(lastTag.parentNode)) {
            lastTag.parentNode.replaceChild(newTag, lastTag)
          }
        }
        result
      }

      new REFrag(rendered)
    }

    class REFrag(rendered: Signal[dom.Node, S])(implicit engine: Engine[S, Turn[S]]) extends Frag {
      def applyTo(t: dom.Element) = t.appendChild(rendered.now)
      def render: dom.Node = rendered.now
    }
  }


  implicit def attrValue[T: AttrValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Engine[S, Turn[S]]): AttrValue[Sig[T]] = new AttrValue[Sig[T]] {
    def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def styleValue[T: StyleValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Engine[S, Turn[S]]): StyleValue[Sig[T]] = new StyleValue[Sig[T]] {
    def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}
