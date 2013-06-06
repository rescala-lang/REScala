package animal.run

// chose version here
//import animal.versions.observer._
import animal.versions.event._
//import animal.versions.signalonly.World
//import animal.versions.signal._

// test
object RunConsole extends App {
  implicit val world = new World
  
  val sleeptime = 100
  val nAnimals = 10
  val nPlants = 3
  
  world batchSpawn(nAnimals, nPlants)

  while (true) {
    println("\n\n\n")
    println(world.timestring)
    println(world.dump) // dumping the board is a bottleneck!
    
    world.tick
    Thread sleep sleeptime
  }
}