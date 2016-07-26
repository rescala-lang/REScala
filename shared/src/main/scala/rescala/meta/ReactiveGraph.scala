package rescala.meta

import rescala.engines.Ticket
import rescala.graph.{PulsingGraphStruct, Reactive, ReevaluationResult}
import rescala.propagation.Turn

class ReactiveGraph[S <: PulsingGraphStruct] {
  private var elements : collection.mutable.Set[Reactive[S]] = collection.mutable.Set()

  def refresh()(implicit ticket: Ticket[S]) : Unit = {
    val elementsSave = elements
    elements = collection.mutable.Set()
    elementsSave.foreach(addReactive)
  }

  private def addReactive(reactive: Reactive[S])(implicit ticket: Ticket[S]) : Unit = {
    if (!elements.contains(reactive)) {
      elements += reactive

      for (incoming <- ticket {
        reactive.bud.incoming(_)
      }) {
        addReactive(incoming)
      }
      for (outgoing <- ticket {
        reactive.bud.outgoing(_)
      }) {
        addReactive(outgoing)
      }
    }
  }

  def toDot(name : String = "ReactiveGraph")(implicit ticket: Ticket[S]) : String = {
    val builder = StringBuilder.newBuilder
    val elementsNumbered = elements.zip(1 to elements.size)
    builder ++= "digraph " + name + "{\n"
    for ((elem, num) <- elementsNumbered) {
      builder ++= "r" + num + "[label=\"" + elem.toString + "\"];\n"
    }
    for ((elem, num) <- elementsNumbered; (out, outNum) <- ticket { elem.bud.outgoing(_) }.flatMap(o => elementsNumbered.find(_._1 == o))) {
      builder ++= "r" + num + " -> r" + outNum + ";\n"
    }
    builder ++= "\n}"
    builder.toString()
  }

  private def findDominators(reactive: Reactive[S])(implicit ticket: Ticket[S]) : Set[Reactive[S]] = {
    val incoming = ticket { reactive.bud.incoming(_) }
    incoming.foldLeft(Set(reactive))((doms, pred) => doms.intersect(findDominators(pred)))
  }

  private def findPostdominators(reactive: Reactive[S])(implicit ticket: Ticket[S]) : Set[Reactive[S]] = {
    val outgoing = ticket { reactive.bud.outgoing(_) }
    outgoing.foldLeft(Set(reactive))((doms, pred) => doms.intersect(findDominators(pred)))
  }

  def isSingleEntryExitArea(entry: Reactive[S], exit: Reactive[S])(implicit ticket: Ticket[S]) : Boolean =
    findDominators(exit).contains(entry) && findPostdominators(entry).contains(exit)

  def mergeReactives(reactives: List[Reactive[S]])(implicit ticket: Ticket[S]) : Reactive[S] = {
    val headBud = reactives.head.bud
    val lastBud = reactives.last.bud
    val newBud = ticket { t => t.bud(lastBud.pulses.get(t), transient = false, headBud.incoming(t)) }

    val newReactive = ticket { t =>
      val newReactive = t.create(headBud.incoming(t))(new Reactive[S] {
      /**
        * Spore that is used to internally manage the reactive evaluation of this value
        *
        * @return Spore for this value
        */
      override protected[rescala] val bud: S#Spore[Reactive[S]] = newBud

      /**
        * Reevaluates this value when it is internally scheduled for reevaluation
        *
        * @param turn Turn that handles the reevaluation
        * @return Result of the reevaluation
        */
      override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
        reactives.foldLeft[ReevaluationResult[S]](ReevaluationResult.Static(false))((_, reactive) => reactive.reevaluate())
      }
    })
      headBud.incoming(t).foreach(_.bud.drop(reactives.head)(t))
      lastBud.outgoing(t).foreach( o => {
        newBud.discover(o)(t)
        lastBud.drop(o)(t)
        o.bud.updateIncoming(o.bud.incoming(t) - reactives.last + newReactive)(t)
      })
      newReactive
    }

    elements --= reactives
    elements += newReactive
    newReactive
  }

}

object ReactiveGraph {
  def fromReactive[S <: PulsingGraphStruct](reactive: Reactive[S])(implicit ticket: Ticket[S]) = {
    val graph = new ReactiveGraph[S]()
    graph.addReactive(reactive)
    graph
  }


}


