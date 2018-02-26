import org.scalajs.dom
import org.scalajs.dom.{DocumentFragment, Node}
import rescala.core.{CreationTicket, Initializer, Scheduler, Struct}
import rescala.reactives.Signals.Diff
import rescala.reactives.{Observe, Signal}

import scala.language.higherKinds
import scalatags.JsDom.all._

package object rescalatags {

  implicit class SignalToScalatags[S <: Struct](signal: Signal[Frag, S]) {
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag(implicit ct: CreationTicket[S], engine: Scheduler[S]): Frag = {
      ct { creation => asFragInner(engine)(creation) }
    }

    private def asFragInner(engine: Scheduler[S])(implicit creation: Initializer[S]): REFrag = {
      val result: Signal[Node, S] = signal
        .map(_.render)
        .recover { case t => span(t.toString).render }
        .withDefault("".render)

      def nodeList(n: Node): List[Node] = {
        if (n.isInstanceOf[DocumentFragment]) List.tabulate(n.childNodes.length)(n.childNodes.apply)
        else List(n)
      }

      @scala.annotation.tailrec
      def replaceAll(parent: Node, old: List[Node], now: List[Node]): Unit = (old, now) match {
        case (o::Nil, n :: (ns @ _ :: _)) if o.nextSibling != null =>
          parent.replaceChild(n, o)
          ns.foreach(parent.insertBefore(_, o.nextSibling))
        case (o::os, n::ns) =>
          parent.replaceChild(n, o)
          replaceAll(parent, os, ns)

        case (Nil, ns) => ns.foreach(parent.appendChild)
        case (os, Nil) => os.foreach(parent.removeChild)
      }

      val observer = Observe.weak(result.change, fireImmediately = false)(
        { case Some(Diff(lastFrag, newFrag)) =>
          val olds = nodeList(lastFrag)
          val news = nodeList(newFrag)
          val parent = olds.head.parentNode
          if (parent != null && !scalajs.js.isUndefined(parent)) {
            replaceAll(parent,olds,news)
          }
        },
        t => throw t)
      new REFrag(result, observer, engine)
    }


    private class REFrag(rendered: Signal[dom.Node, S], val observe: Observe[S], engine: Scheduler[S]) extends Frag {
      def applyTo(t: dom.Element) = t.appendChild(rendered.readValueOnce(engine))
      def render: dom.Node = rendered.readValueOnce(engine)
    }
  }


  implicit def attrValue[T: AttrValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Scheduler[S]): AttrValue[Sig[T]] = new AttrValue[Sig[T]] {
    def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def styleValue[T: StyleValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Scheduler[S]): StyleValue[Sig[T]] = new StyleValue[Sig[T]] {
    def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}
