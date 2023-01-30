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
import scalatags.JsDom.tags2.{article, aside, nav, section}
import kofre.base.Id
import loci.transmitter.RemoteRef
import replication.fbdc.{FbdcExampleData, Req}

import scala.collection.immutable.LinearSeq

object HTML {

  def leftClickHandler(action: => Unit) = { (e: MouseEvent) =>
    if (e.button == 0) {
      e.preventDefault()
      action
    }
  }

  val RemoteRegex = raw"""^remote#(\d+).*""".r.anchored
  def remotePrettyName(rr: RemoteRef) =
    val RemoteRegex(res) = rr.toString: @unchecked
    res

  def connectionManagement(ccm: ContentConnectionManager, fbdcExampleData: FbdcExampleData) = {
    import fbdcExampleData.dataManager
    List(
      h1("connection management"),
      section(table(
        tr(td("total state size"), dataManager.encodedStateSize.map(s => td(s)).asModifier),
        tr(
          td("request queue"),
          dataManager.mergedState.map(v => td(v.store.requests.elements.size)).asModifier
        ),
        tr(
          td("response queue"),
          dataManager.mergedState.map(v => td(v.store.responses.elements.size)).asModifier
        ),
      )),
      section(
        button("disseminate local", onclick := leftClickHandler(dataManager.disseminateLocalBuffer())),
        button("disseminate all", onclick := leftClickHandler(dataManager.disseminateFull()))
      ),
      section(table(
        thead(th("remote ref"), th("connection"), th("request"), th("dots")),
        tr(
          td(Id.unwrap(dataManager.replicaId)),
          td(),
          td(),
          table(
            dataManager.currentContext.map(dotsToRows).asModifierL
          )
        ),
        ccm.connectedRemotes.map { all =>
          all.toList.sortBy(_._1.toString).map { (rr, connected) =>
            tr(
              td(remotePrettyName(rr)),
              if !connected
              then td("disconnected")
              else
                List(
                  td(button("disconnect", onclick := leftClickHandler(rr.disconnect()))),
                  td(button("request", onclick := leftClickHandler(dataManager.requestMissingFrom(rr)))),
                  td(table(
                    dataManager.contextOf(rr).map(dotsToRows).asModifierL
                  ))
                )
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

  def providers(exdat: FbdcExampleData) = {
    div(
      header(h1("make a request")),
      section(table(
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
      ))
    )

  }
}
