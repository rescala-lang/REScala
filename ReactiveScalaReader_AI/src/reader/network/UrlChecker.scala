package reader.network

import scala.events._
import scala.events.behaviour._
import java.net._

class UrlChecker {
  type CheckArg = String
  type CheckResult = Either[String,URL]
  type AfterCheck = (CheckArg,CheckResult)
  
  //val UrlValidity = Var(false)
  //val UrlValid: Signal[Boolean] = Signal{ UrlValidity == true} 
  
  val url = new Var("") 
  
  var UrlValid: Signal[Boolean] = Signal{checkURLSignal(url.toString)}
  var ErrorMessage: Signal[String] = Signal{EM} 

  /**
   * Try to increase confidence that the String is a valid feed url
   * by performing some simple checks.
   *
   * @param url The string to check
   * @return Nothing is returned but events are fired, see below
   */
  val check = Observable( (url: String) => checkURL(url) )

  // Tries to create a url from the string and returns it in Right
  // if not successful, a Left with an error message is returned
  private def checkURL(url: String): CheckResult = {
    try {
      val u = new URL(url)
      u.getContent
//      UrlValidity() = true
      System.out.println("url true!!!!!" + UrlValid.getValue)
      Right(u)
    } catch {
      case e: UnknownHostException => Left(errorMessage(url,e))
      case e: MalformedURLException => Left(errorMessage(url,e))
    }
  }
  
  var EM: String = ""
  
    private def checkURLSignal(url: String): Boolean = {
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

  private lazy val checkSuccessful: Event[CheckResult] =
    check.after && { t: AfterCheck => t._2.isRight } map { t: AfterCheck => t._2 }

  private lazy val checkFailed: Event[CheckResult] =
    check.after && { t: AfterCheck => t._2.isLeft } map { t: AfterCheck => t._2 }

  private lazy val checkedOption: Event[Option[URL]] =
    (checkSuccessful || checkFailed) map { e: CheckResult => e match { case Right(u) => Some(u)
                                                                      case Left(_)  => None } }

  /** Fired for every valid url checked */
  lazy val checkedURL = new ImperativeEvent[URL]
  checkedOption += { opt => opt foreach { checkedURL(_) } }

  /** Only fires if the checked url is valid */
  lazy val urlIsValid: Event[Unit]   = checkSuccessful.dropParam

  /** Only fires if the checked url is invalid */
  lazy val urlIsInvalid: Event[Unit] = checkFailed.dropParam

  private def errorMessage(url: String, e: Exception): String =
    "Error while checking '" + url + "' - " + e.getMessage

}
