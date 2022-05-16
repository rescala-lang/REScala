package todo

import com.github.plokhotnyuk.jsoniter_scala.core._
import org.scalajs.dom
import rescala.default.{Signal, implicitScheduler}

object Storing {

  def storedAs[A: JsonValueCodec](key: String, default: => A)(create: A => Signal[A]): Signal[A] = {
    val init: A = {
      val item = dom.window.localStorage.getItem(key)
      if (item == null) default
      else {
        val res =
          try { readFromString[A](item) }
          catch {
            case cause: Throwable =>
              println(s"could not restore $key: $cause")
              cause.printStackTrace()
              dom.window.localStorage.removeItem(key)
              default
          }
        println(s"restored $res")
        res
      }
    }
    val sig = create(init)
    sig.observe(
      { (ft: A) =>
        println(s"storing $key")
        dom.window.localStorage.setItem(key, writeToString(ft))
      },
      fireImmediately = true
    )
    sig
  }

}
