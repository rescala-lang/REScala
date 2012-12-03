package scala.events.test
import scala.events.behaviour._
import scala.events._
import scala.events.scalareact

object Example extends Application with scalareact.Observing {
  
  val e = new ImperativeEvent[Int]
  val accum = e.fold(0){_ + _}

  // intended signal instancitaion
  val dep = Signal { accum() + 10}
  
  // Has EScala changed event
  accum.changed += {println(_)}
  
  // also possible: scala.react constructors
  val dep2 = scalareact.Strict { lazyA() + 1 }
  val lazyA = scalareact.Lazy { accum() + 1 }
  
  val flowSignal = scalareact.Signal.flow("No occurence"){ self =>
    self await accum
    self() = "First occurence"
    self awaitNext accum
	self() = "Second occurence"
  }
  
  val nChanges = scalareact.Signal.loop(0) { self =>
    self awaitNext accum
    self() = self.previous + 1
  }
  
  
  import scala.events.behaviour.SignalConversions._
  
  nChanges += {x => println("n = " + x)}
  flowSignal += { println(_)}
  
  e(7)
  e(3)
  e(10)
  e(10)
  
  println("turns executed: " + scala.events.scalareact.engine.currentTurn)  
}