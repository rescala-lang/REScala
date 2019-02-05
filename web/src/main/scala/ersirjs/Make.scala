package ersirjs

import ersirjs.Definitions._
import scalatags.JsDom.all._
import scalatags.JsDom.attrs.{onclick, style}
import scalatags.JsDom.tags.a
import scalatags.JsDom.tags2.nav

object Make {

  def imageStyle(fitType: Int): Modifier = {
    def s(mw: Boolean = false, mh: Boolean = false, w: Boolean = false, h: Boolean = false) =
      s"max-height: ${if (mh) "100vh" else "none"}; max-width: ${if (mw) "100vw" else "none"}; height: ${if (h) "100vh" else "auto"}; width: ${if (w) "100vw" else "auto"}"

    style := (fitType % 8 match {
      case 0 => ""
      case 1 => s()
      case 2 => s(mw = true)
      case 3 => s(mh = true)
      case 4 => s(mw = true, mh = true)
      case 5 => s(w = true)
      case 6 => s(h = true)
      case 7 => s(w = true, h = true)
    })
  }

  def fullscreenToggle(stuff: Modifier*): HtmlTag =
    a(if (Definitions.isFullscreen()) Definitions.class_button_active else Definitions.class_button,
      onclick := (() => Definitions.toggleFullscreen()))(stuff: _*)


  def navigation(links: Modifier*): HtmlTag =
    nav(class_button_group)(links :_*)
}
