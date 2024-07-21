package reswing

import scala.swing.{Color, Dimension, Font, MenuBar}

@scala.annotation.nowarn("msg=shadows field")
class ReMenuBar(
    val contents: ReSwingValue[CompList] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize)
    with ReSequentialContainer {
  override protected lazy val peer: MenuBar & ComponentMixin = new MenuBar with ComponentMixin
}

object ReMenuBar {
  implicit def toMenuBar(component: ReMenuBar): MenuBar = component.peer
}
