package rescala.pipelining

import scala.annotation.elidable
import scala.annotation.elidable._

object LogUtils {
  
  @elidable(ASSERTION)
  def println(text : Any) = {
    Console.println(s"${Thread.currentThread().getId}: $text")
  }

}