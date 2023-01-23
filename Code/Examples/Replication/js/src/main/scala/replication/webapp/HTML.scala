package replication.webapp

import org.scalajs.dom
import org.scalajs.dom.html.Element
import org.scalajs.dom.{MouseEvent, document}
import replication.DataManager
import replication.webapp.MetaInfo
import rescala.default.*
import rescala.extra.Tags.*
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{*, given}
import scalatags.JsDom.tags2.{aside, nav, section}
import kofre.base.Id

import scala.collection.immutable.LinearSeq

object HTML {

  def leftClickHandler(action: => Unit) = { (e: MouseEvent) =>
    if (e.button == 0) {
      e.preventDefault()
      action
    }
  }

  def meta(ccm: ContentConnectionManager, dataManager: DataManager[_]) = {
    val connectionStatus = Signal {
      ccm.connectedRemotes.value.size match {
        case 0     => stringFrag(s"disconnected (attempt № ${ccm.connectionAttempt.value})")
        case other => stringFrag(s"$other active")
      }
    }
    section(
      table(
        thead(th("remote ref"), th("connection"), th("request")),
        ccm.connectedRemotes.map { all =>
          all.toList.sortBy(_._1.toString).map { (rr, connected) =>
            tr(
              td(rr.toString),
              if !connected
              then td("disconnected")
              else
                List(
                  td(button("disconnect", onclick := leftClickHandler(rr.disconnect()))),
                  td(button("request", onclick := leftClickHandler(dataManager.requestMissingFrom(rr))))
                )
              ,
            )
          }
        }.asModifierL,
      ),
      aside(
        "remote url: ",
        ccm.wsUri,
        button("connect", onclick := leftClickHandler(ccm.connect()))
      )
    )
  }

  def dotsToRows(dots: kofre.time.Dots) =
    dots.internal.map { (k, v) =>
      tr(td(Id.unwrap(k)), td(v.toString))
    }.toSeq

  def managedData(dm: DataManager[_]) = {
    val lastChanges = dm.changes.last(10)
    section(
      table(
        thead(th("source"), th("dot vector")),
        lastChanges.map {
          _.map { delta =>
            tr(
              td(Id.unwrap(delta.replicaId)),
              td(
                table(
                  dotsToRows(delta.anon.context): _*
                )
              )
            )
          }
        }.asModifierL
      ),
      button("disseminate", onclick := leftClickHandler(dm.disseminate()))
    )

  }
}
