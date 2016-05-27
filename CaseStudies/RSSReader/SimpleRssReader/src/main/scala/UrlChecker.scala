import rescala._
import rescala._
import java.net._

class UrlChecker(val url: String) {

  var EM: String = ""

  var UrlValid: Signal[Boolean] = Signal{checkURL(url)}
  var ErrorMessage: Signal[String] = Signal{EM}

    private def checkURL(url: String): Boolean = {
      var valid = false
    try {
      val u = new URL(url)
      u.getContent
      Right(u)
      valid = true
    } catch {
      case e: UnknownHostException => EM = errorMessage(url,e)
      case e: MalformedURLException => EM = errorMessage(url,e)
    }
    return valid
  }

    private def errorMessage(url: String, e: Exception): String =
    "Error while checking '" + url + "' - " + e.getMessage

}
