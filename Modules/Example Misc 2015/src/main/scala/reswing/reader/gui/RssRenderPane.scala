package reswing.reader.gui

import reactives.default.*
import reswing.reader.data.RSSItem

import java.awt.Desktop
import java.io.IOException
import javax.swing.event.{HyperlinkEvent, HyperlinkListener}
import scala.swing.EditorPane

/** Displays the content of an single RSS Item */
class RssItemRenderPane(item: Signal[Option[RSSItem]]) extends EditorPane {
  super.editable = false
  super.contentType = "text/html"

  peer.addHyperlinkListener(new HyperlinkListener() {
    def hyperlinkUpdate(e: HyperlinkEvent) =
      if (e.getEventType == HyperlinkEvent.EventType.ENTERED)
        peer.setToolTipText(e.getDescription)
      else if (e.getEventType == HyperlinkEvent.EventType.EXITED)
        peer.setToolTipText(null)
      else if (e.getEventType == HyperlinkEvent.EventType.ACTIVATED)
        try Desktop.getDesktop.browse(e.getURL.toURI)
        catch {
          case e: IOException => e.printStackTrace
        }
  })

  item.changed observe { // #IF //#HDL
    _ match {
      case Some(item) =>
        text = List(
          "<b>Title:</b>",
          item.title,
          "<b>Publication date:</b>",
          item.pubDate.getOrElse("Unknown"),
          "<b>Source:</b>",
          item.link.map { link => "<a href=" + link + ">" + link + "</a>" }.getOrElse("Unknown"),
          "<hr>",
          item.description
        ).mkString("<br>")
      case None =>
        text = ""
    }
  }
}
