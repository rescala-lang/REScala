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
import replication.fbdc.{FbdcExampleData, Req}

import scala.collection.immutable.LinearSeq

object HTML {

  def leftClickHandler(action: => Unit) = { (e: MouseEvent) =>
    if (e.button == 0) {
      e.preventDefault()
      action
    }
  }

  def connectionManagement(ccm: ContentConnectionManager, dataManager: DataManager[_]) = {
    val connectionStatus = Signal {
      ccm.connectedRemotes.value.size match {
        case 0     => stringFrag(s"disconnected (attempt № ${ccm.connectionAttempt.value})")
        case other => stringFrag(s"$other active")
      }
    }
    div(
      header(h1("connection management")),
      section(button("disseminate", onclick := leftClickHandler(dataManager.disseminate()))),
      section(table(
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
      )),
      section(aside(
        "remote url: ",
        ccm.wsUri,
        button("connect", onclick := leftClickHandler(ccm.connect()))
      ))
    )
  }

  def dotsToRows(dots: kofre.time.Dots) =
    dots.internal.map { (k, v) =>
      tr(td(Id.unwrap(k)), td(v.toString))
    }.toSeq

  def debugInfo(dm: DataManager[_]) = {
    val lastChanges = dm.changes.last(10)
    div(
      header(h1("dot vector debug info")),
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
    )

  }

  def providers(exdat: FbdcExampleData) = {
    div(
      header(h1("make a request")),
      table(
        thead(th("provider"), th("tasks")),
        exdat.providers.map { prov =>
          prov.observeRemoveMap.entries.map { (id, provided) =>
            tr(
              td(Id.unwrap(id)),
              td(
                provided.elements.iterator.map(name =>
                  button(
                    name,
                    onclick := leftClickHandler {
                      exdat.dataManager.transform { curr =>
                        curr.modReq { reqs =>
                          reqs.enqueue(Req.Fortune(id))
                        }
                      }
                    }
                  )
                ).toList: _*
              )
            )
          }.toList

        }.asModifierL
      )
    )

  }
}
