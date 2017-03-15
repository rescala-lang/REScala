import java.net.URL

import rescala._

object Main extends App {

  val urlCheck = new UrlChecker("http://www.faz.net/aktuell/rhein-main/?rssview=1")
  System.out.println(urlCheck.UrlValid.now)
  System.out.println(urlCheck.ErrorMessage.now)

  val url1 = new URL("http://www.faz.net/aktuell/rhein-main/?rssview=1")

  val fetch = new Fetcher(url1)
  System.out.println(fetch.listOfUrl.now)
  System.out.println(fetch.ChTitle.now)

  val g = new GUI
  g.main(Array())


}
