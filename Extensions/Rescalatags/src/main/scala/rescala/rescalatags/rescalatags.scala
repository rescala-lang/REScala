package rescala
import org.scalajs.dom
import org.scalajs.dom.{DocumentFragment, Element, Node}
import rescala.core.{CreationTicket, Scheduler, Struct}
import rescala.reactives.{Observe, Signal, Var}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic

import scala.language.higherKinds
import scala.scalajs.js

package object rescalatags extends RescalatagsLowPriorityimplicits {

  implicit def transformTypedTag[S <: Struct, Out <: Element]: TypedTag[Out] => Out = _.render


  implicit class SignalToScalatags[S <: Struct, In, Out <: Node](signal: Signal[In, S])
                                                                (implicit transform: In => Out) {
    private[this] type ResultFrag = generic.Frag[Element, Out]
    /**
      * converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom
      */
    def asFrag(implicit engine: Scheduler[S]): ResultFrag = {
      new REFrag(signal, engine)
    }


    private class REFrag(rendered: Signal[In, S], engine: Scheduler[S]) extends ResultFrag {
      var observe: Observe[S] = null
      var currentTag: Out = _
      override def applyTo(parent: Element): Unit = {
        CreationTicket.fromEngine(engine).transaction { init =>

          currentTag = transform(init.accessTicket().now(rendered))
          parent.appendChild(currentTag)

          observe = Observe.weak(rendered, fireImmediately = false)(
            { newTag =>
              val newNode = transform(newTag)
              val olds = nodeList(currentTag)
              val news = nodeList(newNode)
              if (parent != null && !scalajs.js.isUndefined(parent)) {
                replaceAll(parent, olds, news)
              }
              currentTag = newNode
            },
             t => throw t)(init)
        }


      }
      override def render: Out = ???
    }
  }

  def genericReactiveAttrValue[T: AttrValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Scheduler[S])
  : AttrValue[Sig[T]] = new AttrValue[Sig[T]] {
    def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }

  implicit def varAttrValue[T: AttrValue, S <: Struct](implicit engine: Scheduler[S])
  : AttrValue[Var[T, S]] = genericReactiveAttrValue[T, S, ({type λ[T2] = Var[T2, S]})#λ]

  implicit def signalAttrValue[T: AttrValue, S <: Struct](implicit engine: Scheduler[S])
  : AttrValue[Signal[T, S]] = genericReactiveAttrValue[T, S, ({type λ[T2] = Signal[T2, S]})#λ]

  def genericReactiveStyleValue[T: StyleValue, S <: Struct, Sig[T2] <: Signal[T2, S]](implicit engine: Scheduler[S])
  : StyleValue[Sig[T]] = new StyleValue[Sig[T]] {
    def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
      signal.observe { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }

  implicit def varStyleValue[T: StyleValue, S <: Struct](implicit engine: Scheduler[S])
  : StyleValue[Var[T, S]] = genericReactiveStyleValue[T, S, ({type λ[T2] = Var[T2, S]})#λ]

  implicit def signalStyleValue[T: StyleValue, S <: Struct](implicit engine: Scheduler[S])
  : StyleValue[Signal[T, S]] = genericReactiveStyleValue[T, S, ({type λ[T2] = Signal[T2, S]})#λ]


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

}

trait RescalatagsLowPriorityimplicits {

  implicit def bindEvt[T, S <: Struct](implicit scheduler: Scheduler[S]) = new generic.AttrValue[dom.Element, rescala.reactives.Evt[T, S]]{
    def apply(t: dom.Element, a: generic.Attr, v: rescala.reactives.Evt[T, S]): Unit = {
      t.asInstanceOf[js.Dynamic].updateDynamic(a.name)((e: T) => v.fire(e))
    }
  }

  implicit def transformFragment[S <: Struct, Out <: Element]: Frag => Node = _.render
}
