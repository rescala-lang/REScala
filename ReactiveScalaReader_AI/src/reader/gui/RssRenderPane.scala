package reader.gui

import swing._
import reader.data._

import javax.swing.event._
import java.io.IOException
import java.awt.Desktop

/**
* Displays the content of an single RSS Item
*/
class RssItemRenderPane extends EditorPane {
  super.editable = false
  super.contentType = "text/html"

  peer.addHyperlinkListener(new HyperlinkListener() {
    def hyperlinkUpdate(e: HyperlinkEvent) {
      if (e.getEventType() == HyperlinkEvent.EventType.ENTERED) {
        peer.setToolTipText(e.getDescription())
      } else if (e.getEventType() == HyperlinkEvent.EventType.EXITED) {
        peer.setToolTipText(null)
      } else if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
        try {
            Desktop.getDesktop().browse(e.getURL().toURI)
        } catch {
          case e: IOException => e.printStackTrace()
        }
      }
    }
  })

  def content_=(item: RSSItem) {
    super.text = renderRSSItem(item)
  }

  private def renderRSSItem(i: RSSItem): String = {
    List( "<b>Title:</b>"
        , i.title
        , "<b>Publication date:</b>"
        , i.pubDate.getOrElse("Unknown")
        , "<b>Source:</b>"
        , i.link.map { link => "<a href="+link+">"+link+"</a>"}.getOrElse("Unknown")
        , "<hr>"
        , i.description
    ).mkString("<br>")
  }

}
