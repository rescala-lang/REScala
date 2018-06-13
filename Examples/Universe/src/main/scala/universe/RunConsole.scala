package universe

import java.nio.file.{Files, Paths, StandardOpenOption}

import rescala.reactives.Observe


object RunConsole {
  def main(args: Array[String]): Unit = {
    val nAnimals = 100
    val nPlants = 300
    val width = 100
    val height = 100
    val repetitions = 10
    val threadCounts = Range.inclusive(1, 16)

    val outfile = s"universe-${Globals.engineName}.csv"

    Files.write(Paths.get(outfile),
      s""""Repetition","Threads","Score","Param: engineName","Benchmark","Param: height","Param: width","Param: animals","Param: plants"${"\n"}""".getBytes(),
      StandardOpenOption.WRITE, StandardOpenOption.CREATE)

    for (repetition <- 0 to repetitions; threads <- threadCounts) {
      println(s"rep: $repetition, threads: $threads")

      Observe.dereferenceAllStrongObserversWARNINGonlyUseThisIfYouKnowWhatYouAreDoing()
      System.gc()
      Globals.setParallelism(threads)

      val world = new World(width, height)

      world batchSpawn(nAnimals, nPlants)

      val start = System.nanoTime()
      import universe.Globals.engine
      while (world.time.week.readValueOnce < 2) {
        world.tick()
        world.runPlan()
      }
      //      println(world.time)
      //      println(world.status)
      //      println(world.board.dump)
      val duration = (System.nanoTime() - start) / 1E9d
      if (repetition > 0) Files.write(Paths.get(outfile),
        s"""$repetition,$threads,$duration,"${Globals.engineName}","UniverseCaseStudy",$height,$width,$nAnimals,$nPlants${"\n"}""".getBytes(),
        StandardOpenOption.APPEND)
    }
  }

}


object RunPrinting {
  def main(args: Array[String]): Unit = {
    val nAnimals = 100
    val nPlants = 300
    val width = 70
    val height = 20
    val repetitions = 10

    Observe.dereferenceAllStrongObserversWARNINGonlyUseThisIfYouKnowWhatYouAreDoing()
    System.gc()
    Globals.setParallelism(1)

    val world = new World(width, height)

    world batchSpawn(nAnimals, nPlants)

    val start = System.nanoTime()
    import universe.Globals.engine
    world.time.day.observe{ _ =>
      println(world.time)
      println(world.status)
      println(world.board.dump)
    }
    while (world.time.week.readValueOnce < 2) {
      world.tick()
      world.runPlan()

    }
    val duration = (System.nanoTime() - start) / 1E9d
  }

}
