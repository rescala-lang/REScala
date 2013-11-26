package examples.signaltests
import scala.events.behaviour._
import scala.events._

class Foo(val member : Int) {
  def multiply(x : Int) = x * member
  def double() = 2 * member
}

class Number(val value : Int) {
  def double() = 2 * value
}

object HigherOrderTests extends Application {
    import scala.events.behaviour.SignalConversions._
    
    /** This sadly does not work */
    //val unlifted = (x : Int) => 2 * x
	//val implicitly_lifted : (Reactive[Int] => Reactive[Int]) = unlifted
  
    
    /** Explicit lifting */
	val v = Var(0)
	def double(x : Int) : Int = 2 * x	
	val lifted = Signal.lift(double)
	val doubled_x = lifted(v)
	

	
	v() = 2
	println(doubled_x())
	
	//val test = new Foo(3)
	//val lifted_multiply = Signal.lift(test.multiply)	
	//println(lifted_multiply(v)())
    
    
    
    // Mouse example
    
    val click = new ImperativeEvent[(Int, Int)]
    
    def mean(list : List[(Int,Int)]) : (Double, Double) = {
      val (x, y) = list.unzip
      val n = list.length + 1.0
      return (x.sum / n, y.sum / n)
    }
    val meanR = Signal.lift(mean)
    
    
    import scalareact._
    
    val history = click.last(5)
    val meanMouse = meanR(history)
    val meanConstant = meanR(List((10, 0), (10, 42)))
 
	
    meanMouse.changed += {println(_)}
    click((12, 13))
    click((120, 13))
   
}