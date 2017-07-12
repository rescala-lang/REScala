import rescala._

import scala.collection.immutable.HashMap

/**
  * Created by julian on 05.07.17.
  */
object main {
  def main(args: Array[String]): Unit = {
    DistributionEngine.host = "Host1"

    val a = Var(CIncOnlyCounter(11))
    DistributionEngine.publish("moppi", a)

    DistributionEngine.host = "Host2"
    val b = Var(CIncOnlyCounter(13))
    DistributionEngine.publish("moppi", b)
    println(b.now.payload)
    
    b.transform(_.increase)

    DistributionEngine.host = "Host1"
    //a.set(a.now.increase)
    //b.set(b.now.increase)
    println(a.now)
    println(b.now)
   
    

    val counter: Signal[CIncOnlyCounter] =  e.fold(CIncOnlyCounter(13)) { (c, _) => c.increase }
    
    val syncedCounter: Signal[CIncOnlyCounter] = DistributionEngine.publish("moppi", counter)
    
    
    DistributionEngine.host = "Host2"
    val otherCounter = DistributionEngine.subscribe(counter)
    
    
    
  

    /**
    DistributionEngine.host = "Host3"
    val c = Var(CIncOnlyCounter(0))
    DistributionEngine.publish("moppi", c)
      **/
  }
}
