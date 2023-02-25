package reswing.reader

import java.net.URL

import scala.io.Source
import scala.swing.Dialog
import scala.swing.Dialog.Message
import scala.swing.Swing
import scala.swing.Swing.EmptyIcon

import rescala.default._
import reswing.reader.connectors.CentralizedEvents
import reswing.reader.connectors.SimpleReporter
import reswing.reader.data.FeedStore
import reswing.reader.data.RSSItem
import reswing.reader.data.XmlParser
import reswing.reader.gui.GUI
import reswing.reader.network.Fetcher
import reswing.reader.network.UrlChecker

object Main extends App {
  val tick    = Evt[Unit]() // #EVT
  val checker = new UrlChecker
  val fetcher = new Fetcher(checker.checkedURL.fold(Set.empty[URL])(_ + _))
  val parser  = new XmlParser
  val store   = new FeedStore(parser.channelParsed, parser.itemParsed)
  val app = new GUI(
    store,
    (store.itemAdded map { (x: RSSItem) => // #EF
      (x.srcChannel map (_.title) getOrElse "<unknown>") + ": " + x.title
    }) hold "",      // #IF
    Signal.dynamic { // #SIG
      val itemCount = (store.channels.value map { case (_, items) => items.value.size }).sum
      "Channels: " + store.channels.value.size + " Items: " + itemCount
    },
    fetcher.state
  )

  setupGuiEvents()

  List(SimpleReporter, CentralizedEvents) foreach { m =>
    m.mediate(fetcher, parser, store, checker)
  }

  checker.urlIsInvalid observe { _ => showInvalidUrlDialog() } // #HDL

  val sleepTime = 5000L // 20000

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
    Seq("http://www.faz.net/aktuell/politik/?rssview=1", "http://feeds.gawker.com/lifehacker/full")

  def showInvalidUrlDialog() =
    Dialog.showMessage(null, "This url is not valid", "Invalid url", Message.Error, EmptyIcon)

  private def setupGuiEvents(): Unit = {
    app.requestURLAddition observe { url =>
      checker.check(url); ()
    } // #HDL

    val guardedTick = tick && { _ => app.refreshAllowed.value } // #EVT //#EF

    (app.refresh || guardedTick) observe { _ => fetcher.fetchAll() } // #EF //#HDL
    ()
  }

  private def loadURLs(path: String): Option[Seq[String]] = {
    println("trying to load from " + path)
    val res =
      try Some(Source.fromFile(path).getLines().toList)
      catch { case _: Throwable => None }
    println("result: " + res)
    res
  }
}
