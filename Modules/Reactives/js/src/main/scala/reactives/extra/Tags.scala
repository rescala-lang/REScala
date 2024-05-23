package reactives.extra

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{Element, KeyboardEvent, MutationObserver, Node, Range, console, document}
import reactives.core.{CreationTicket, Disconnectable, DynamicScope, PlanTransactionScope, Scheduler, Tracing}
import reactives.operator.*
import reactives.structure.RExceptions.ObservedException
import reactives.structure.{Observe, Pulse}

import console.log as println

import scala.annotation.targetName
import scala.scalajs.js

object Tags {

  trait RangeSplice[-A <: dom.Element, -T]:
    def splice(anchor: A, range: dom.Range, value: T): Unit
  object RangeSplice:
    given elem: RangeSplice[dom.Element, dom.Element] with {
      override def splice(anchor: dom.Element, range: Range, value: dom.Element) =
        range.insertNode(value)
    }
    given many[A <: dom.Element, T](using other: RangeSplice[A, T]): RangeSplice[A, Seq[T]] with {
      override def splice(anchor: A, range: Range, value: Seq[T]) =
        value.reverseIterator.foreach(v => other.splice(anchor, range, v))
    }
    given string: RangeSplice[dom.Element, String] with {
      override def splice(anchor: dom.Element, range: Range, value: String) =
        anchor.textContent = value
    }

  extension [A <: dom.Element](anchor: A)
    def reattach[T](signal: Signal[T], removeOnContainerMismatch: Boolean = false)(using
        splicer: RangeSplice[A, T],
        creationTicket: CreationTicket[Interface.State]
    ): anchor.type = {
      val range = document.createRange()
      range.selectNodeContents(anchor)
      range.collapse(toStart = false)
      Observe.strong(signal, true) {
        tagObserver(anchor, signal) { v =>
          println("branch ", range.commonAncestorContainer != anchor)
          println("else ", range.commonAncestorContainer == anchor)

          if range.commonAncestorContainer != anchor then
            println(
              "weird state",
              anchor,
              range,
              range.commonAncestorContainer,
              range.startContainer,
              range.endContainer
            )

          if range.commonAncestorContainer == anchor || !removeOnContainerMismatch then
            range.extractContents()
            splicer.splice(anchor, range, v)
        }
      }
      anchor
    }

  extension (input: Input)
    def inputEntered(using
        creationTicket: CreationTicket[Interface.State],
        scheduler: PlanTransactionScope[Interface.State]
    ): Event[String] = {
      val handler: Event.CBR[KeyboardEvent, Unit] = Event.fromCallback(input.onkeyup = Event.handle(_))

      handler.event
        .map { (e: KeyboardEvent) =>
          if e.key == "Enter" then
            val res = input.value.trim
            if res.nonEmpty then
              e.preventDefault()
              input.value = ""
              Some(res)
            else None
          else None
        }.flatten
    }

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

  def tagObserver[A](
      parent: dom.Element,
      rendered: Signal[A]
  )(fun: A => Unit)(reevalVal: Pulse[A]): Observe.ObserveInteract =
    new Observe.ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty(_) | Pulse.NoChange => false
          case Pulse.Exceptional(f) =>
            throw ObservedException(rendered, s"signal tag attached to $parent observed", f)
          case Pulse.Value(v) =>
            isInDocumentHack(parent)(v)
        }
      }

      override def execute(): Unit =
        reevalVal match {
          case Pulse.empty(_) | Pulse.NoChange => ()
          case Pulse.Value(v) =>
            fun(v)
          case Pulse.Exceptional(f) =>
            throw new IllegalStateException("should have aborted earlier", f)
        }
    }

}
