import org.scalajs.dom
import org.scalajs.dom.{DocumentFragment, Element, Node}
import rescala.core.{CreationTicket, Initializer, Scheduler, Struct}
import rescala.reactives.Signals.Diff
import rescala.reactives.{Observe, Signal}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic

import scala.language.higherKinds

package object rescalatags extends lowPriorityimplicits {

  implicit def transformTypedTag[S <: Struct, Out <: Element]
    (implicit ct: CreationTicket[S]): Signal[TypedTag[Out], S] => Signal[Out, S] = _.map(_.render)


  implicit class SignalToScalatags[S <: Struct, In, Out <: Node](signal: Signal[In, S])(implicit transform: Signal[In, S] => Signal[Out, S]) {
    private[this] type ResultFrag = generic.Frag[Element, Out]
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag(implicit ct: CreationTicket[S], engine: Scheduler[S]): ResultFrag = {
      ct.transaction { init => asFragInner(engine)(init) }
    }

    private def asFragInner(engine: Scheduler[S])(implicit creation: Initializer[S]): REFrag = {
      val result: Signal[Out, S] = transform(signal)
      val observer = makeObserver(result)
      new REFrag(result, observer, engine)
    }


    private class REFrag(rendered: Signal[Out, S], val observe: Observe[S], engine: Scheduler[S]) extends ResultFrag {
      override def applyTo(t: Element): Unit = t.appendChild(rendered.readValueOnce(engine))
      override def render: Out = rendered.readValueOnce(engine)
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


  // helper functions

  @scala.annotation.tailrec
  private def replaceAll(parent: Node, old: List[Node], now: List[Node]): Unit = (old, now) match {
    case (o :: Nil, n :: (ns@_ :: _)) if o.nextSibling != null =>
      parent.replaceChild(n, o)
      ns.foreach(parent.insertBefore(_, o.nextSibling))
    case (o :: os, n :: ns) =>
      parent.replaceChild(n, o)
      replaceAll(parent, os, ns)

    case (Nil, ns) => ns.foreach(parent.appendChild)
    case (os, Nil) => os.foreach(parent.removeChild)
  }


  private def nodeList(n: Node): List[Node] = {
    if (n.isInstanceOf[DocumentFragment]) List.tabulate(n.childNodes.length)(n.childNodes.apply)
    else List(n)
  }

  private def makeObserver[S <: Struct](result: Signal[Node, S])(implicit creation: Initializer[S]): Observe[S] = {
    Observe.weak(result.change, fireImmediately = false)(
      { case Some(Diff(lastFrag, newFrag)) =>
        val olds = nodeList(lastFrag)
        val news = nodeList(newFrag)
        val parent = olds.head.parentNode
        if (parent != null && !scalajs.js.isUndefined(parent)) {
          replaceAll(parent, olds, news)
        }
      },
       t => throw t)
  }

}

trait lowPriorityimplicits {
  implicit def transformFragment[S <: Struct, Out <: Element]
    (implicit ct: CreationTicket[S]): Signal[Frag, S] => Signal[Node, S] = signal =>
    signal
    .map(_.render)
    .recover { case t => span(t.toString).render }
    .withDefault("".render)
}
