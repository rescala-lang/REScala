import java.net.URL

object Main extends App {
 
  val urlCheck = new UrlChecker("http://www.faz.net/aktuell/rhein-main/?rssview=1")
  System.out.println(urlCheck.UrlValid)
  System.out.println(urlCheck.ErrorMessage)
  
  val url1 = new URL("http://www.scala-lang.org/featured/rss.xml")
  val url2 = new URL("http://www.faz.net/aktuell/rhein-main/?rssview=1")
  
  val fetch = new Fetcher(url1)
  System.out.println(fetch.listOfUrl)
  System.out.println(fetch.ChTitle)

  
}