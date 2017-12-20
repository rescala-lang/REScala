package examples.timeElapsing

import rescala._
import rescala._




object TimeElapsing extends App {

  println("start!")



  val tick = Var(0)
  val second = Signal{ tick() % 60 }
  val minute = Signal{ tick()/60 % 60 }
  val hour = Signal{ tick()/(60*60) % 60 }
  val day = Signal{ tick()/(60*60*24) % 24 }

  // Note, day is still circular. At some point day==0 again
  // What if one wants that minutes depend on seconds ?

  while(true){
    Thread.sleep(0)
    println((second.now, minute.now, hour.now, day.now))
    tick.set(tick.now + 1)
  }

}



