package reader

import reader.common.implicits._
import reader.data._
import reader.connectors._
import reader.gui._
import reader.network._

import scala.events._

import scala.swing._
import scala.swing.Swing._
import scala.swing.Dialog._

object Main extends App {
  val tick = new ImperativeEvent[Unit]

  val fetcher = new Fetcher
  val parser = new XmlParser
  val store = new FeedStore
  val app = new GUI(store)
  val checker = new UrlChecker

  
  
  setupGuiEvents()

  List(SimpleReporter, CentralizedEvents).foreach { m =>
    m.mediate(fetcher, parser, store, checker)
  }

  checker.urlIsInvalid += { _ => showInvalidUrlDialog(); System.out.println(checker.UrlValid.getValue) }
  checker.urlIsValid += { _ => System.out.println(checker.UrlValid.getValue)}

  val sleepTime = 20000

  // ---------------------------------------------------------------------------

  println("Program started")

  app.main(Array())

  val readUrls: Option[Seq[String]] = for { file <- args.headOption
                                            urls <- loadURLs(file)
                                          } yield urls

  readUrls.getOrElse(defaultURLs).foreach(fetcher.addURL(_))

  while (true) { tick(); Thread.sleep(sleepTime) }

  // ---------------------------------------------------------------------------

  def defaultURLs: Seq[String] = 
    Seq( "http://www.faz.net/aktuell/politik/?rssview=1"
       , "https://bitbucket.org/ipls/feedtest/rss?token=e529f098840207ac321eedac745fa5df"
       , "http://feeds.gawker.com/lifehacker/full"
       , "http://www.scala-lang.org/featured/rss.xml"
       )

  def showInvalidUrlDialog() {
    Dialog.showMessage(null, "This url is not valid", "Invalid url", Message.Error, EmptyIcon)
  }

  private def setupGuiEvents() {
    app.requestURLAddition += { url => checker.check(url) }

    fetcher.startedFetching += { _ => app.notifications("Started fetching.") }
    fetcher.startedFetching += { _ => app.refreshButton.enabled = false }

    fetcher.finishedFetching += { _ => app.notifications("Finished fetching.") }
    fetcher.finishedFetching += { _ => app.refreshButton.enabled = true }

    store.itemAdded += { x => app.notifications(x.srcChannel.map(_.title).getOrElse("<unknown>") + ": " + x.title) }
    store.channelsChanged || store.itemAdded += { _ =>
      {
        val itemCount = store.channels.foldLeft(0)((total, channel) => total + store.itemsFor(channel).get.size)
        app.itemStatus("Channels: " + store.channels.size + " Items: " + itemCount)
      }
    }

    val guardedTick = tick && { _ => app.refreshAllowed }
    (app.refresh || guardedTick) += { _ => fetcher.trigger() }
  }

  private def loadURLs(path: String): Option[Seq[String]] = {
    println("trying to load from " + path)
    scala.io.Source.fromFile(path).getLines.toList
    val res = try Some(scala.io.Source.fromFile(path).getLines.toList) catch { case _ => None }
    println("result: " + res)
    res
  }

}
