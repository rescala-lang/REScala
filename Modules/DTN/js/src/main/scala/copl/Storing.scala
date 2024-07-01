package copl

import com.github.plokhotnyuk.jsoniter_scala.core._
import org.scalajs.dom
import rescala.default.{Signal, implicitScheduler}

object Storing {

  def storedAs[A: JsonValueCodec](key: String, default: => A)(create: A => Signal[A]): Signal[A] = {
    val init: A =
      try { readFromString[A](dom.window.localStorage.getItem(key)) }
      catch {
        case _: Throwable =>
          dom.window.localStorage.removeItem(key)
          default
      }
    val sig = create(init)
    sig.observe(
      { (ft: A) =>
        dom.window.localStorage.setItem(key, writeToString(ft))
      },
      fireImmediately = false
    )
    sig
  }

}
