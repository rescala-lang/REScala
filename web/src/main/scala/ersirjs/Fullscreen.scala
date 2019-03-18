package ersirjs

object Fullscreen {

  private val dDocument = scala.scalajs.js.Dynamic.global.document
  private def getDefined[T](ts: T*): Option[T] = ts.find(v => v != null && !scalajs.js.isUndefined(v))


  def isFullscreen(): Boolean =
    getDefined(dDocument.fullscreenElement,
               dDocument.webkitFullscreenElement,
               dDocument.mozFullScreenElement,
               dDocument.msFullscreenElement).isDefined

  def toggleFullscreen(): Unit = {
    val de = dDocument.documentElement


    def requestFullscreen = getDefined(
      de.requestFullscreen,
      de.msRequestFullscreen,
      de.mozRequestFullScreen,
      de.webkitRequestFullscreen)

    def exitFullscreen = getDefined(
      dDocument.exitFullscreen,
      dDocument.webkitExitFullscreen,
      dDocument.mozCancelFullScreen,
      dDocument.msExitFullscreen)

    if (isFullscreen()) exitFullscreen.foreach(_.call(dDocument)) else requestFullscreen.foreach(_.call(de))
  }
}
