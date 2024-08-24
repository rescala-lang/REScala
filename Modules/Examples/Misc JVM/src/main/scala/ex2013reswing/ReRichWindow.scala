package ex2013reswing

import scala.swing.RichWindow

trait ReRichWindow extends ReWindow {
  protected def peer: RichWindow

  def title: ReSwingValue[String]

  title.using({ () => peer.title }, peer.title_=, "title")

  size.force("resizable", peer.resizable_=, false)
  bounds.force("resizable", peer.resizable_=, false)
}

object ReRichWindow {
  implicit def toRichWindow(component: ReRichWindow): RichWindow = component.peer
}
