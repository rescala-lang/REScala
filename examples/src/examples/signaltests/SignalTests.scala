package examples.signaltests
import scala.events.behaviour._
import scala.events._


object SignalTests extends Application {
  val a = Var(0)
                    
  // state can only be expressed by a variable outside the Signal, this is ugly
  var state = 0
  val accum2 : Signal[(Int,Int)]  = Signal {
     state += 1
	 (a(), state)
   }
  
  // fold hides the state
  val accum = a.changed.fold(0) {_ + _}
  val numChanges = a.changed.iterate(0){_ + 1}
  val delayed = accum delay 1
  
  def mean[T <: Numeric[T]](value : Signal[Int]) : Signal[Double] = {
    val n = value.changed.iterate(0) {_ + 1}
    val sum = value.changed.fold(0) {_ + _}
    Signal {sum() / (n() + 1.0)}
    Signal { 23 }
  }  

 
  val foo = Signal { accum() }
  val fooChanged = foo.changed
  fooChanged += {println(_)}
  
  val n =  a.changed.iterate(0)(_ + 1)
  val sum = a.changed.fold(0) {_ + _}
  
  
  val average = Signal {sum() / (n() + 1.0)}
  
  a() = 4
  a() = 2
  a() = 3
  println("accum = " + accum.getValue)
  println("changes = " + numChanges.getValue)
  println("delayed = " + delayed.getValue)
  println("average = " + average.getValue)
  
  
  //val celsius : Signal[Int] = Signal { (fahrenheit() - 32) * 5 / 9 }
  //val fahrenheit : Signal[Int] = Signal { celsius() * 9 / 5 + 32 }
}
