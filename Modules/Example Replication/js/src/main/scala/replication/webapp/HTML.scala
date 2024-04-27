package replication.webapp

import loci.transmitter.RemoteRef
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html.Element
import rdts.base.Uid
import reactives.default.*
import reactives.extra.Tags.*
import replication.DataManager
import replication.fbdc.{FbdcExampleData, Req}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.{aside, section}

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
        tr(td("total state size")).render.reattach(dataManager.encodedStateSize.map(s => td(s).render)),
        tr(
          td("request queue"),
        ).render.reattach(dataManager.mergedState.map(v => td(v.data.requests.elements.size).render)),
      )),
      section(
        button("disseminate local", onclick := leftClickHandler(dataManager.disseminateLocalBuffer())),
        button("disseminate all", onclick   := leftClickHandler(dataManager.disseminateFull()))
      ),
      section(table(
        thead(th("remote ref"), th("connection"), th("request"), th("dots")),
        tr(
          td(dataManager.replicaId.show),
          td(),
          td(),
          table().render.reattach(dataManager.currentContext.map(dotsToRows))
        )
      ).render.reattach(ccm.connectedRemotes.map { all =>
        all.toList.sortBy(_._1.toString).map { (rr, connected) =>
          tr(
            td(remotePrettyName(rr)),
            if !connected
            then td("disconnected")
            else
              List(
                td(button("disconnect", onclick := leftClickHandler(rr.disconnect()))),
                td(button(
                  "request",
                  onclick := leftClickHandler {
                    dataManager.requestMissingFrom(rr)
                    ()
                  }
                )),
                td(table().render.reattach(
                  dataManager.contextOf(rr).map(dotsToRows)
                ))
              )
          ).render
        }
      })),
      section(aside(
        "remote url: ",
        ccm.wsUri,
        button("connect", onclick := leftClickHandler(ccm.connect()))
      ))
    )
  }

  def dotsToRows(dots: rdts.time.Dots) =
    dots.internal.toList.sortBy(t => Uid.unwrap(t._1)).map { (k, v) =>
      tr(td(Uid.unwrap(k)), td(v.toString)).render
    }.toSeq

  def providers(exdat: FbdcExampleData) = {
    div(
      h1("make a request"),
    ).render.reattach(
      exdat.providers.map { prov =>
        prov.observeRemoveMap.entries.map { (id, provided) =>
          section(
            header(h2("Executor:", Uid.unwrap(id))),
            provided.elements.iterator.map {
              case "fortune" => fortuneBox(exdat, id)
              case other     => northwindBox(exdat, id)
            }.toList
          ).render
        }.toList
      }
    )

  }

  def fortuneBox(exdat: FbdcExampleData, id: Uid) = aside(
    button(
      "get fortune",
      onclick := leftClickHandler {
        exdat.dataManager.modReq { reqs =>
          reqs.enqueue(using exdat.dataManager.replicaId)(Req.Fortune(id))
        }
      }
    ),
  ).render.reattach(exdat.latestFortune.map(f => p(f.map(_.result).getOrElse("")).render))

  def northwindBox(exdat: FbdcExampleData, id: Uid) =
    val ip = input().render

    aside(
      ip,
      button(
        "query northwind",
        onclick := leftClickHandler {
          exdat.dataManager.modReq { reqs =>
            reqs.enqueue(using exdat.dataManager.replicaId)(Req.Northwind(id, ip.value))
          }
        }
      ),
      p(
        table().render.reattach(
          exdat.latestNorthwind.map {
            case None => Nil
            case Some(res) =>
              val keys = res.result.head.keys.toList.sorted
              thead(keys.map(th(_)).toList*).render ::
              res.result.map { row =>
                tr(keys.map(k => td(row(k)))).render
              }
          }
        )
      )
    ).render
}
