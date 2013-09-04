package examples.scalatests

class Wrapper(val value : Int)
object Wrapper {
  implicit def asWrapped(x : Int) = new Wrapper(x)
}


object ImplicitTest extends Application {
  def onWrapped(x : Wrapper) = println(x.value)  
  // this converts implicitly
  onWrapped(42)
  
}