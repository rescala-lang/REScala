package reswing

import scala.language.implicitConversions
import scala.swing.RichWindow

trait ReRichWindow extends ReWindow {
  protected def peer: RichWindow
  
  def title: ReSwingValue[String]

  title using (peer.title _, peer.title_= _, "title")
  
  size force("resizable", peer.resizable_=, false)
  bounds force("resizable", peer.resizable_=, false)
}

object ReRichWindow {
  implicit def toRichWindow(component: ReRichWindow): RichWindow = component.peer
}
