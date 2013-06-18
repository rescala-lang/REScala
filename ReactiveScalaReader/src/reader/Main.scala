package reader

import scala.events.ImperativeEvent
import scala.io.Source
import scala.swing.Dialog
import scala.swing.Dialog.Message
import scala.swing.Swing
import scala.swing.Swing.EmptyIcon

import reader.common.implicits.stringToUrl
import reader.connectors.CentralizedEvents
import reader.connectors.SimpleReporter
import reader.data.FeedStore
import reader.data.RSSItem
import reader.data.XmlParser
import reader.gui.GUI
import reader.network.Fetcher
import reader.network.UrlChecker

object Main extends App {
  val tick = new ImperativeEvent[Unit]
  val checker = new UrlChecker
  val fetcher = new Fetcher
  val parser = new XmlParser
  val store = new FeedStore
  val app = new GUI(
      store,
      fetcher.startedFetching ||
      fetcher.finishedFetching ||
      (store.itemAdded map { x: RSSItem =>
        (x.srcChannel map (_.title) getOrElse "<unknown>") + ": " + x.title }),
      store.contentChanged map { _: Unit =>
        val itemCount = (store.channels map { c => (store itemsFor c).get.size }).sum
        "Channels: " + store.channels.size + " Items: " + itemCount
      })
  
  setupGuiEvents
  
  List(SimpleReporter, CentralizedEvents) foreach { m =>
    m.mediate(fetcher, parser, store, checker)
  }
  
  checker.urlIsInvalid += { _ => showInvalidUrlDialog }
  
  val sleepTime = 20000
  
  // ---------------------------------------------------------------------------
  
  println("Program started")
  
  app.main(Array())
  
  val readUrls: Option[Seq[String]] = for {
    file <- args.headOption
    urls <- loadURLs(file)
  } yield urls
  
  (readUrls getOrElse defaultURLs) foreach (checker.check(_))
  
  while (true) { Swing.onEDTWait { tick() }; Thread.sleep(sleepTime) }
  
  // ---------------------------------------------------------------------------
  
  def defaultURLs: Seq[String] = 
    Seq("http://www.faz.net/aktuell/politik/?rssview=1",
        "http://feeds.gawker.com/lifehacker/full",
        "http://www.scala-lang.org/featured/rss.xml")
  
  def showInvalidUrlDialog =
    Dialog.showMessage(null, "This url is not valid", "Invalid url", Message.Error, EmptyIcon)
  
  private def setupGuiEvents {
    app.requestURLAddition += { url => checker.check(url) }
    
    val guardedTick = tick && { _ => app.refreshAllowed }
    
    (app.refresh || guardedTick) += { _ => fetcher.fetchAll }
  }
  
  private def loadURLs(path: String): Option[Seq[String]] = {
    println("trying to load from " + path)
    val res = try Some(Source.fromFile(path).getLines.toList) catch { case _: Throwable => None }
    println("result: " + res)
    res
  }
}
