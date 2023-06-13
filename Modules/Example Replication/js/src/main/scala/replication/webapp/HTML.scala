package replication.webapp

import org.scalajs.dom
import org.scalajs.dom.html.Element
import org.scalajs.dom.{MouseEvent}
import replication.DataManager
import rescala.default.*
import rescala.extra.Tags.*
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.{aside, section}
import kofre.base.Uid
import loci.transmitter.RemoteRef
import replication.fbdc.{FbdcExampleData, Req}

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
          dataManager.mergedState.map(v => td(v.data.requests.elements.size)).asModifier
        ),
      )),
      section(
        button("disseminate local", onclick := leftClickHandler(dataManager.disseminateLocalBuffer())),
        button("disseminate all", onclick   := leftClickHandler(dataManager.disseminateFull()))
      ),
      section(table(
        thead(th("remote ref"), th("connection"), th("request"), th("dots")),
        tr(
          td(Uid.unwrap(dataManager.replicaId)),
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
                  td(button("request", onclick := leftClickHandler{
                    dataManager.requestMissingFrom(rr)
                    ()
                  })),
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
    dots.internal.toList.sortBy(t => Uid.unwrap(t._1)).map { (k, v) =>
      tr(td(Uid.unwrap(k)), td(v.toString))
    }.toSeq

  def providers(exdat: FbdcExampleData) = {
    div(
      h1("make a request"),
      exdat.providers.map { prov =>
        prov.observeRemoveMap.entries.map { (id, provided) =>
          section(
            header(h2("Executor:", Uid.unwrap(id))),
            provided.elements.iterator.map {
              case "fortune" => fortuneBox(exdat, id)
              case other     => northwindBox(exdat, id)
            }.toList
          ).asInstanceOf[TypedTag[Element]]
        }.toList
      }.asModifierL
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
    exdat.latestFortune.map(f => p(f.map(_.result).getOrElse(""))).asModifier
  )

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
        table(
          exdat.latestNorthwind.map {
            case None => Nil
            case Some(res) =>
              val keys = res.result.head.keys.toList.sorted
              thead(keys.map(th(_)).toList: _*) ::
              res.result.map { row =>
                tr(keys.map(k => td(row(k))))
              }
          }.asModifierL
        )
      )
    )
}
