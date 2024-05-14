package replication.webapp

import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html.TableRow
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

  def connectionManagement(ccm: ContentConnectionManager, fbdcExampleData: FbdcExampleData) = {
    import fbdcExampleData.dataManager
    List(
      h1("connection management"),
      section(table(
        tr(td("self id"), td(dataManager.replicaId.show)),
        tr(td("total state size")).render.reattach(dataManager.encodedStateSize.map(s => td(s).render)),
        tr(td("tick")).render.reattach(dataManager.tick.map(td(_).render)),
        tr(
          td("request queue"),
        ).render.reattach(dataManager.mergedState.map(v => td(v.data.requests.elements.size).render)),
      )),
      section(
        button("disseminate local", onclick := leftClickHandler(dataManager.disseminateLocalBuffer())),
        button("disseminate all", onclick   := leftClickHandler(dataManager.disseminateFull()))
      ),
      section(table(
        thead(th("remote ref"), th("dots")),
        tr(
          td(dataManager.replicaId.show),
          td(table().render.reattach(dataManager.currentContext.map(dotsToRows)))
        )
      ).render.reattach(Signal {
        val peers = dataManager.peerids.value
        peers.toList.map: peer =>
          tr(
            td(peer.show),
            td(table().render.reattach(dataManager.contextOf(peer).map(dotsToRows)))
          ).render
      })),
      section(aside(
        "remote url: ",
        ccm.wsUri,
        button("connect", onclick := leftClickHandler(ccm.connect()))
      ))
    )
  }

  def dotsToRows(dots: rdts.time.Dots): List[TableRow] =
    dots.internal.toList.sortBy(t => Uid.unwrap(t._1)).map { (k, v) =>
      tr(td(Uid.unwrap(k)), td(v.toString)).render
    }

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
