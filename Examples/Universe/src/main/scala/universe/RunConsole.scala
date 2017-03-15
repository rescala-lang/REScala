package universe

import java.nio.file.{Files, Paths, StandardOpenOption}



object RunConsole {
  def main(args: Array[String]): Unit = {
    val nAnimals = 100
    val nPlants = 300
    val width = 100
    val height = 100
    val repetitions = 5
    val threadCounts = Range.inclusive(1,16)

    val outfile = s"universe-${Globals.engineName}.csv"

    Files.write(Paths.get(outfile),
      s""""Threads","Score","Param: engineName","Benchmark","Param: height","Param: width","Param: animals","Param: plants"${"\n"}""".getBytes(),
      StandardOpenOption.WRITE, StandardOpenOption.CREATE)

    for (repetition <- 0 to repetitions; threads <- threadCounts) {
      println(s"rep: $repetition, threads: $threads")

      Globals.setParallelism(threads)

      val world = new World(width, height)

      world batchSpawn(nAnimals, nPlants)

      val start = System.nanoTime()
      while (world.time.week.now(universe.Globals.engine) < 2) {
        world.tick()
        world.runPlan()
      }
      //    println(world.time)
      //    println(world.status)
      //    println(world.board.dump)
      val duration = (System.nanoTime() - start) / 1000000000.0D
      if (repetition > 0) Files.write(Paths.get(outfile),
        s"""$threads,$duration,"${Globals.engineName}","UniverseCaseStudy",$height,$width,$nAnimals,$nPlants${"\n"}""".getBytes(),
        StandardOpenOption.APPEND)
    }
  }
}
