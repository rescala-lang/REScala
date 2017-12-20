import org.scalajs.dom
import org.scalajs.dom.Node
import rescala.core.{Creation, CreationTicket, Engine, Struct}
import rescala.reactives.Signals.Diff
import rescala.reactives.{Observe, Signal}

import scala.language.higherKinds
import scalatags.JsDom.all._

package object rescalatags {

  implicit class SignalToScalatags[S <: Struct](signal: Signal[Tag, S]) {
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag(implicit ct: CreationTicket[S], engine: Engine[S]): Frag = {
      ct { creation => asFragInner(engine)(creation) }
    }

    private def asFragInner(engine: Engine[S])(implicit creation: Creation[S]): REFrag = {
      val result: Signal[Node, S] = signal
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
      new REFrag(result, observer, engine)
    }


    private class REFrag(rendered: Signal[dom.Node, S], val observe: Observe[S], engine: Engine[S]) extends Frag {
      def applyTo(t: dom.Element) = t.appendChild(rendered.now(engine))
      def render: dom.Node = rendered.now(engine)
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
