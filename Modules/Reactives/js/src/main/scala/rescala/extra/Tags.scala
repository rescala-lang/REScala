package rescala.extra

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{Element, Node}
import rescala.core.{CreationTicket, Disconnectable, DynamicScope, Tracing}
import scalatags.JsDom.all.{Attr, AttrValue, Modifier, Style, StyleValue}
import scalatags.JsDom.{StringFrag, TypedTag}
import scalatags.generic
import scalatags.jsdom.Frag
import rescala.operator.Interface
import rescala.structure.RExceptions.ObservedException
import rescala.structure.{Observe, Pulse}

import scala.scalajs.js

object Tags extends Tags[rescala.default.type](rescala.default, true)

class Tags[Api <: Interface](val api: Api, val addDebuggingIds: Boolean) {
  import api._
  def isInDocument(element: Element): Boolean = {
    js.Dynamic.global.document.contains(element).asInstanceOf[Boolean]
  }

  /* This only returns true the second time it is called to prevent observers to directly trigger */
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
    def asModifier(implicit engine: DynamicScope[BundleState]): Modifier = {
      new REFragModifier(signal, engine)
    }
  }

  implicit class SignalStrToScalatags(val signal: Signal[StringFrag]) {

    /** converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom */
    def asModifier(implicit engine: DynamicScope[BundleState]): Modifier = {
      new REFragModifier(signal, engine)
    }
  }

  implicit class SignalTagListToScalatags(val signal: Signal[Seq[TypedTag[Element]]]) {

    /** converts a Signal of a scalatags Tag to a scalatags Frag which automatically reflects changes to the signal in the dom */
    def asModifierL(implicit engine: DynamicScope[BundleState]): Modifier = {
      new RETagListModifier(signal.withDefault(Nil)(engine), engine)
    }
  }

  private class REFragModifier(rendered: Signal[Frag], engine: DynamicScope[BundleState]) extends Modifier {
    var observe: Disconnectable = null
    var currentNode: Node       = null
    override def applyTo(parent: Element): Unit = {
      CreationTicket.fromExplicitDynamicScope(engine).scope.embedTransaction { init =>
        if (observe != null) {
          observe.disconnect()
          if (currentNode != null) {
            currentNode.parentNode.removeChild(currentNode)
            currentNode = null
          }
        }

        observe = Observe.strong(rendered, fireImmediately = true)(
          tagObserver[Frag](parent, rendered) { newTag =>
            // println(s"$rendered parent $parent")
            if (parent != null && !scalajs.js.isUndefined(parent)) {
              val newNode = newTag.render
              Tracing.observe(Tracing.DomAssociation(rendered, Tracing.RawWrapper(newNode)))
              if addDebuggingIds
              then
                newNode match
                  case elem: dom.Element =>
                    elem.setAttribute("data-rescala-resource-id", rendered.info.idCounter.toString)
                  case other =>
                    parent.setAttribute(s"data-rescala-resource-child-${rendered.info.idCounter.toString}", "true")
              // println(s"$rendered appending $newNode to $parent with $currentNode")
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
  )(fun: A => Unit)(reevalVal: Pulse[A]): Observe.ObserveInteract =
    new Observe.ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty | Pulse.NoChange => false
          case Pulse.Exceptional(f) =>
            throw ObservedException(rendered, s"signal tag attached to $parent observed", f)
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

  private class RETagListModifier(rendered: Signal[Seq[TypedTag[Element]]], scheduler: DynamicScope[BundleState])
      extends Modifier {
    var observe: Disconnectable             = null
    var currentNodes: Seq[Element]          = Nil
    var currentTags: Seq[TypedTag[Element]] = Nil
    override def applyTo(parent: Element): Unit = {
      scheduler.dynamicTransaction { tx =>
        if (observe == null) {
          currentTags = tx.now(rendered)
          currentNodes = currentTags.map(_.render)
          currentNodes.foreach(parent.appendChild)
          if addDebuggingIds
          then parent.setAttribute(s"data-rescala-resource-child-${rendered.info.idCounter.toString}", "true")
          Tracing.observe(Tracing.DomAssociation(rendered, Tracing.RawWrapper(parent)))
        } else {
          // println(s"Warning, added $rendered to dom AGAIN, this is experimental")
          observe.disconnect()
          observe = null
          // adding nodes to the dom again should move them
          currentNodes.foreach(parent.appendChild)
        }

        observe = Observe.strong(rendered, fireImmediately = false) {
          tagObserver(parent, rendered) { newTags =>
            // println(s"$rendered parent $parent")
            if (parent != null && !scalajs.js.isUndefined(parent)) {
              currentNodes = replaceAll(parent, currentNodes, currentTags, newTags)
              currentTags = newTags
            }
          }
        }(tx)
      }
    }
  }

  implicit def genericReactiveAttrValue[T: AttrValue, Sig[T2] <: Signal[T2]](implicit
      engine: DynamicScope[BundleState]
  ): AttrValue[Sig[T]] =
    new AttrValue[Sig[T]] {
      def apply(t: dom.Element, a: Attr, signal: Sig[T]): Unit = {
        Observe.strong(signal, fireImmediately = true)(tagObserver(t, signal) { value =>
          // hack because scalatags seems to add classes instead of replacing
          t.removeAttribute(a.name)
          // hack to also change the current value of an input, not just the value attribute …
          if (a.name == "value" && t.isInstanceOf[Input] && value.isInstanceOf[String])
            t.asInstanceOf[Input].value = value.asInstanceOf[String]
          implicitly[AttrValue[T]].apply(t, a, value)
        })(engine)
        ()
      }
    }

  // implicit def varAttrValue[T: AttrValue](implicit scheduler: Scheduler)
  // : AttrValue[Var[T]] = genericReactiveAttrValue[T, S, ({type λ[T2] = Var[T2]})#λ]
  //
  // implicit def signalAttrValue[T: AttrValue](implicit scheduler: Scheduler)
  // : AttrValue[Signal[T]] = genericReactiveAttrValue[T, S, ({type λ[T2] = Signal[T2]})#λ]

  def genericReactiveStyleValue[T, Sig[T2] <: Signal[T2]](implicit
      engine: DynamicScope[BundleState],
      tstyle: StyleValue[T]
  ): StyleValue[Sig[T]] =
    new StyleValue[Sig[T]] {
      def apply(t: dom.Element, s: Style, signal: Sig[T]): Unit = {
        Observe.strong(signal, fireImmediately = true)(tagObserver(t, signal)({ value =>
          tstyle.apply(t, s, value)
        }))(engine)
        ()
      }
    }

  implicit def varStyleValue[T: StyleValue](implicit ds: DynamicScope[BundleState]): StyleValue[Var[T]] =
    genericReactiveStyleValue[T, ({ type λ[T2] = Var[T2] })#λ](ds, implicitly)

  implicit def signalStyleValue[T: StyleValue](implicit ds: DynamicScope[BundleState]): StyleValue[Signal[T]] =
    genericReactiveStyleValue[T, ({ type λ[T2] = Signal[T2] })#λ](ds, implicitly)



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
    // println(s"replacing for $parent")
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
        ()
      }
    } else {
      while (nti.hasNext) {
        val nn = nti.next().render
        newNodes ::= nn
        parent.appendChild(nn)
        ()
      }
    }
    while (oni.hasNext)
      parent.removeChild(oni.next())
      ()

    newNodes.reverse
  }

}
