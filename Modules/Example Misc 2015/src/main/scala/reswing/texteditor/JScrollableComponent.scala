package reswing.texteditor

import java.awt.{Cursor, Dimension, Rectangle}
import javax.swing.{JComponent, JViewport, Scrollable, SwingConstants, UIManager}

class JScrollableComponent extends JComponent with Scrollable {
  setFont(UIManager.getFont("TextField.font"))
  setCursor(new Cursor(Cursor.TEXT_CURSOR))

  val metrics    = getFontMetrics(getFont)
  val unitHeight = metrics.getHeight
  val unitWidth  = metrics.charWidth('m')

  override def setPreferredSize(preferredSize: Dimension): Unit = {
    super.setPreferredSize(preferredSize)
    if getParent != null then
      getParent.doLayout()
  }

  def getPreferredScrollableViewportSize = getPreferredSize

  def getScrollableTracksViewportHeight =
    getParent.isInstanceOf[JViewport] && getParent.asInstanceOf[JViewport].getHeight > getPreferredSize.height
  def getScrollableTracksViewportWidth =
    getParent.isInstanceOf[JViewport] && getParent.asInstanceOf[JViewport].getWidth > getPreferredSize.width

  def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int) =
    if orientation == SwingConstants.HORIZONTAL then visibleRect.width else visibleRect.height

  def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int) =
    if orientation == SwingConstants.HORIZONTAL then unitWidth else unitHeight
}
