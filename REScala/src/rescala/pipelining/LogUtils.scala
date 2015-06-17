package rescala.pipelining

object LogUtils {
  
  def println(text : Any) = {
    Console.println(s"${Thread.currentThread().getId}: $text")
  }

}