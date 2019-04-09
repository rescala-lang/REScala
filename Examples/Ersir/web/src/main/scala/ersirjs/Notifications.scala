package ersirjs

import org.scalajs.dom.experimental.Notification

object Notifications {
  def send(str: String): Unit = {
    Notification.permission match {
      case "granted" => new Notification(str)
      case "denied"  =>
      case other     => Notification.requestPermission(_ => send(str))

    }
  }
}
