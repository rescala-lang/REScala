package reader

import java.net.URL

import scala.language.implicitConversions

object common {
  def ??? : Nothing = throw new Error("Implementation missing")
  type ??? = Nothing

  def debug(s: String) = println(s)

  object implicits {
    implicit def stringToUrl(s: String): URL = new URL(s)
  }

  def sequence[A](l : List[Option[A]]) =
    if (l contains None) None else Some(l.flatten)
}
