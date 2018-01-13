package reader

import java.net.URL

import scala.io.Source
import scala.swing.Dialog
import scala.swing.Dialog.Message
import scala.swing.Swing
import scala.swing.Swing.EmptyIcon

import rescala._
import rescala._
import rescala._
import reader.connectors.CentralizedEvents
import reader.connectors.SimpleReporter
import reader.data.FeedStore
import reader.data.RSSItem
import reader.data.XmlParser
import reader.gui.GUI
import reader.network.Fetcher
import reader.network.UrlChecker

object Main extends App {
  val tick = Evt[Unit]  //#EVT
  val checker = new UrlChecker
  val fetcher = new Fetcher(checker.checkedURL.fold(Set.empty[URL])(_ + _))
  val parser = new XmlParser
  val store = new FeedStore(parser.channelParsed, parser.itemParsed)
  val app = new GUI(
      store,
      (store.itemAdded map { x: RSSItem => //#EF
        (x.srcChannel map (_.title) getOrElse "<unknown>") + ": " + x.title }) latest "", //#IF
      Signal.dynamic { //#SIG
        val itemCount = (store.channels() map { case (_, items) => items().size }).sum
        "Channels: " + store.channels().size + " Items: " + itemCount
      },
      fetcher.state)

  setupGuiEvents

  List(SimpleReporter, CentralizedEvents) foreach { m =>
    m.mediate(fetcher, parser, store, checker)
  }

  checker.urlIsInvalid += { _ => showInvalidUrlDialog } //#HDL

  val sleepTime = 5000l //20000

  // ---------------------------------------------------------------------------

  println("Program started")

  app.main(Array())

  val readUrls: Option[Seq[String]] = for {
    file <- args.headOption
    urls <- loadURLs(file)
  } yield urls

  (readUrls getOrElse defaultURLs) foreach (checker.check(_))

  while (true) { Swing.onEDTWait { tick.fire() }; Thread.sleep(sleepTime) }

  // ---------------------------------------------------------------------------

  def defaultURLs: Seq[String] =
    Seq("http://www.faz.net/aktuell/politik/?rssview=1",
        "http://feeds.gawker.com/lifehacker/full")

  def showInvalidUrlDialog() =
    Dialog.showMessage(null, "This url is not valid", "Invalid url", Message.Error, EmptyIcon)

  private def setupGuiEvents(): Unit = {
    app.requestURLAddition += { url => checker.check(url) } //#HDL

    val guardedTick = tick && { _ => app.refreshAllowed.now } //#EVT //#EF

    (app.refresh || guardedTick) += { _ => fetcher.fetchAll } //#EF //#HDL
  }

  private def loadURLs(path: String): Option[Seq[String]] = {
    println("trying to load from " + path)
    val res = try Some(Source.fromFile(path).getLines.toList) catch { case _: Throwable => None }
    println("result: " + res)
    res
  }
}
