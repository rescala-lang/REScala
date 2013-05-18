package scala.events
import scala.react.Domain

/**
 * The domain object for Scala.React which has to be imported by all REScala modules
 */
object scalareact extends Domain {
  
  class SingleTurnEngine extends Engine {
	var allowTurn = true
	var turns = 0
	
	def doAndTurn(op: => Unit) {
	  this synchronized {
		  if(allowTurn){
		    allowTurn = false
		    op
		    runTurn
		    
		    // we need to run some additional turns for nested propagations
		    for (_ <- 1 to turns) runTurn

		    allowTurn = true
		    turns = 0
		  }
		  else {
		    turns += 1
		    op
		  }
		}
	}
  }
  
  
  val engine = new SingleTurnEngine
  val scheduler = new ManualScheduler
 }


