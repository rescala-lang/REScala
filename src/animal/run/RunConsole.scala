package animal.run

import animal.universe.World

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
    println(world.status)
    println(world.dump) // dumping the board is a bottleneck!
    
    world.tick()
    Thread sleep sleeptime
  }
}
