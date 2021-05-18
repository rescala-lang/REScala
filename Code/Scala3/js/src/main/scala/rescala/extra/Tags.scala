package rescala.extra

import org.scalajs.dom
import org.scalajs.dom.{Element, Node}
import rescala.interface.RescalaInterface
import scalatags.JsDom.all.{Attr, AttrValue, Modifier, Style, StyleValue}
import scalatags.JsDom.{StringFrag, TypedTag}
import scalatags.generic
import scalatags.jsdom.Frag
import rescala.operator.Pulse
import rescala.operator.RExceptions.ObservedException


import scala.scalajs.js

class Tags(val api: RescalaInterface) {
  import api._

  def isInDocument(element: Element): Boolean = {
    js.Dynamic.global.document.contains(element).asInstanceOf[Boolean]
  }

  def isInDocumentHack(elem: dom.Element): Any => Boolean = {
    var second = false
    _ => {
      if (second) {
        !isInDocument(elem)
      } else {
        second = true
        false
      }
    }
  }

  implicit class SignalToScalatags(val signal: Signal[TypedTag[Element]]) {

    /** converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom */
    def asModifier(implicit engine: Scheduler): Modifier = {
      new REFragModifier(signal, engine)
    }
  }

  implicit class SignalStrToScalatags(val signal: Signal[StringFrag]) {

    /** converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom */
    def asModifier(implicit engine: Scheduler): Modifier = {
      new REFragModifier(signal, engine)
    }
  }

  implicit class SignalTagListToScalatags(val signal: Signal[Seq[TypedTag[Element]]]) {

    /** converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom */
    def asModifierL(implicit engine: Scheduler): Modifier = {
      new RETagListModifier(signal.withDefault(Nil), engine)
    }
  }

  private class REFragModifier(rendered: Signal[Frag], engine: Scheduler) extends Modifier {
    var observe: Observe = null
    var currentNode: Node   = null
    override def applyTo(parent: Element): Unit = {
      CreationTicket.fromScheduler(engine).transaction { init =>
        if (observe != null) {
          observe.remove()(engine)
          if (currentNode != null) {
            currentNode.parentNode.removeChild(currentNode)
            currentNode = null
          }
        }

        observe = Observe.strong(rendered, fireImmediately = true)(
          tagObserver[Frag](parent, rendered) { newTag =>
            //println(s"$rendered parent $parent")
            if (parent != null && !scalajs.js.isUndefined(parent)) {
              val newNode = newTag.render
              //println(s"$rendered appending $newNode to $parent with $currentNode")
              if (currentNode != null) parent.replaceChild(newNode, currentNode)
              else parent.appendChild(newNode)
              currentNode = newNode
            }
          }
        )(init)
      }
    }
  }

  def tagObserver[A](
      parent: dom.Element,
      rendered: Signal[A]
  )(fun: A => Unit)(reevalVal: Pulse[A]): ObserveInteract =
    new ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty | Pulse.NoChange => false
          case Pulse.Exceptional(f) =>
            throw new ObservedException(rendered, s"signal tag attached to $parent observed", f)
          case Pulse.Value(v) =>
            isInDocumentHack(parent)(v)
        }
      }

      override def execute(): Unit =
        reevalVal match {
          case Pulse.empty | Pulse.NoChange => ()
          case Pulse.Value(v) =>
            fun(v)
          case Pulse.Exceptional(f) =>
            throw new IllegalStateException("should have aborted earlier", f)
        }
    }

  private class RETagListModifier(rendered: Signal[Seq[TypedTag[Element]]], engine: Scheduler)
      extends Modifier {
    var observe: Observe                 = null
    var currentNodes: Seq[Element]          = Nil
    var currentTags: Seq[TypedTag[Element]] = Nil
    override def applyTo(parent: Element): Unit = {
      CreationTicket.fromScheduler(engine).transaction { init =>
        if (observe == null) {
          currentTags = init.accessTicket().now(rendered)
          currentNodes = currentTags.map(_.render)
          currentNodes.foreach(parent.appendChild)
        } else {
          //println(s"Warning, added $rendered to dom AGAIN, this is experimental")
          observe.remove()(engine)
          observe = null
          // adding nodes to the dom again should move them
          currentNodes.foreach(parent.appendChild)
        }

        observe = Observe.strong(rendered, fireImmediately = false) {
          tagObserver(parent, rendered) { newTags =>
            println(s"$rendered parent $parent")
            if (parent != null && !scalajs.js.isUndefined(parent)) {
              currentNodes = replaceAll(parent, currentNodes, currentTags, newTags)
              currentTags = newTags
            }
          }
        }(init)
      }
    }
  }

  implicit def genericReactiveAttrValue[T: AttrValue, Sig[T2] <: Signal[T2]](implicit
      engine: Scheduler
  ): AttrValue[Sig[T]] =
    new AttrValue[Sig[T]] {
      def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
        Observe.strong(signal, fireImmediately = true)(tagObserver(t, signal) { value =>
          implicitly[AttrValue[T]].apply(t, a, value)
        })
      }
    }

  //implicit def varAttrValue[T: AttrValue](implicit engine: Scheduler)
  //: AttrValue[Var[T]] = genericReactiveAttrValue[T, S, ({type λ[T2] = Var[T2]})#λ]
  //
  //implicit def signalAttrValue[T: AttrValue](implicit engine: Scheduler)
  //: AttrValue[Signal[T]] = genericReactiveAttrValue[T, S, ({type λ[T2] = Signal[T2]})#λ]

  def genericReactiveStyleValue[T, Sig[T2] <: Signal[T2]](implicit
      engine: Scheduler,
      tstyle: StyleValue[T]
  ): StyleValue[Sig[T]] =
    new StyleValue[Sig[T]] {
      def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
        Observe.strong(signal, fireImmediately = true)(tagObserver(t, signal)({ value =>
          tstyle.apply(t, s, value)
        }))
      }
    }

  implicit def varStyleValue[T: StyleValue](implicit engine: Scheduler): StyleValue[Var[T]] =
    genericReactiveStyleValue[T, ({ type λ[T2] = Var[T2] })#λ]

  implicit def signalStyleValue[T: StyleValue](implicit engine: Scheduler): StyleValue[Signal[T]] =
    genericReactiveStyleValue[T, ({ type λ[T2] = Signal[T2] })#λ]

  implicit def bindEvt[T](implicit scheduler: Scheduler): generic.AttrValue[Element, Evt[T]] =
    new generic.AttrValue[dom.Element, Evt[T]] {
      def apply(t: dom.Element, a: generic.Attr, v: Evt[T]): Unit = {
        t.asInstanceOf[js.Dynamic].updateDynamic(a.name)((e: T) => v.fire(e))
      }
    }

  implicit def optionAttrValue[T](implicit ev: AttrValue[T]): generic.AttrValue[Element, Option[T]] =
    new AttrValue[Option[T]] {
      override def apply(t: Element, a: Attr, v: Option[T]): Unit = {
        v match {
          case Some(value) => ev.apply(t, a, value)
          case None =>
            a.namespace match {
              case None     => t.removeAttribute(a.name)
              case Some(ns) => t.removeAttributeNS(ns.uri, a.name)
            }
        }
      }
    }

  // helper functions

  // this horrible contraption tries to iterate over the list of old tags that should be currently in the dom as nodes.
  // it compares that with the list of new tags that are the target state, and replaces non matching values accordingly.
  private def replaceAll(
      parent: Node,
      oldNodes: Seq[Element],
      oldTags: Seq[TypedTag[Element]],
      newTags: Seq[TypedTag[Element]]
  ): List[Element] = {
    //println(s"replacing for $parent")
    val oni           = oldNodes.iterator
    val oti           = oldTags.iterator
    val nti           = newTags.iterator
    var newNodes      = List[Element]()
    var last: Element = null
    while (nti.hasNext && oti.hasNext) {
      val on = oni.next()
      last = on
      val ot = oti.next()
      val nt = nti.next()
      if (ot != nt) {
        val nn = nt.render
        newNodes ::= nn
        parent.replaceChild(nn, on)
        last = nn
      } else {
        newNodes ::= on
      }
    }
    val nextSibling = if (last != null) last.nextSibling else null
    if (nextSibling != null) {
      while (nti.hasNext) {
        val nn = nti.next().render
        newNodes ::= nn
        parent.insertBefore(nn, nextSibling)
      }
    } else {
      while (nti.hasNext) {
        val nn = nti.next().render
        newNodes ::= nn
        parent.appendChild(nn)
      }
    }
    while (oni.hasNext) parent.removeChild(oni.next())

    newNodes.reverse
  }

}
