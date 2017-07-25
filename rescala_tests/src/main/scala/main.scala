import akka.actor.ActorSystem
import statecrdts._

/**
  * Created by julian on 05.07.17.
  */
object main {
  def main(args: Array[String]): Unit = {
    val system: ActorSystem = ActorSystem("crdtTestbench")

    val m1 = Vertex("[Alice] Hello Bob!")
    var log = RGA[String]()
    log = log.addRight(start, m1)
    val m2 = Vertex("[Bob] Hey Alice!")
    val m3 = Vertex("[Alice] How are you?")
    log = log addRight(m1, m2) addRight(m2, m3)
    println(log.value)

    log = log.remove(m2)
    println(log.before(m1, m2))
    println(log.before(m2, m1))
    println(log.before(m1, m3))
    println(log.value)

    system.terminate()
    //println(r.successor(start).value)
    //println(r.before(start,end))
    //println(r.before(end,start))
    /*
        val lookupServer: ActorRef = system.actorOf(LookupServer.props, "lookupServer")
        val host1: ActorRef = system.actorOf(DistributionEngine.props("Host1", lookupServer), "Host1")
        val host2: ActorRef = system.actorOf(DistributionEngine.props("Host2", lookupServer), "Host2")

        val c = CCounter(host1, "moppi", 12)
        c.increase
        println(c.value)
        println()

        val d = CCounter(host2, "moppi", 0)
        val doubledMoppi = Signal {
          c.signal().value + d.signal().value
        }
    */
    /*
    doubledMoppi.observe(v => println("Observed: " + v))
    d.increase

    println(s"doubledMoppi: ${doubledMoppi.now}")
    println(c.value)
    println(d.value)
    println()

    //Thread sleep 10000
    println(s"doubledMoppi: ${doubledMoppi.now}")
    println(c.value)
    println(d.value)

    println(r.vertices)
    println(r.edges)
    println(r.successor(start).value)
    println(r.before(start, end))


    system.terminate()

    */


    // ###### Old stuff ######

    /*Engine.host = "Host1"

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
*/

    /*
    val system: ActorSystem = ActorSystem("crdtTestbench")

    val l: ActorRef = system.actorOf(LookupServer.props, "lookupServer")
    val host1: ActorRef = system.actorOf(DistributionEngine.props("Host1", l), "Host1")
    val host2: ActorRef = system.actorOf(DistributionEngine.props("Host2", l), "Host2")

    val a = Var(CIncOnlyCounter(11))
    host1 ! Publish("moppi", a)

    val b = Var(CIncOnlyCounter(0))
    host2 ! Publish("moppi", b)

    b.transform(_.increase)
    */


    /*
    var c = ORSet(1, 2, 3)
    println(c)
    var d = c
    println(d)
    c = c.remove(3)
    d = d.add(3)
    d = d merge c
    println(d)
    println(d.payload)
    d = d.fromValue(d.value)
    println(d.payload)
    */
    /*

    This should work:

    val counter: Signal[CIncOnlyCounter] =  e.fold(CIncOnlyCounter(13)) { (c, _) => c.increase }

    val syncedCounter: Signal[CIncOnlyCounter] = DistributionEngine.publish("moppi", counter)


    DistributionEngine.host = "Host2"
    val otherCounter = DistributionEngine.subscribe(counter)

    */


    /**
      *DistributionEngine.host = "Host3"
      * val c = Var(CIncOnlyCounter(0))
      *DistributionEngine.publish("moppi", c)
      **/
  }
}
