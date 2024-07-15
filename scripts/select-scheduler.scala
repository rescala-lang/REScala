#!/usr/bin/env scala-cli
//> using scala 3.4.2
//> using dep de.rmgk.slips::options:0.9.0
//> using dep de.rmgk.slips::script:0.9.0
import java.nio.file.Path
import java.nio.file.Files
import de.rmgk.options

import java.nio.file.StandardOpenOption
import de.rmgk.script.*

import scala.util.matching.Regex

object SelectScheduler {
  def main(args: Array[String]): Unit =
    options.parseArguments(args.toList):
      val selection = options.positional[String]("scheduler", "default scheduler for all").value

      val targetFolder =
        val cwd     = Path.of("").toAbsolutePath
        val project = if cwd.endsWith("scripts") then cwd.getParent else cwd
        project.resolve("Modules/Reactives/shared/src/main/scala/reactives/scheduler/")

      val sourceFile = targetFolder.resolve("GlobalCandidate.scala")

      if !Files.isRegularFile(sourceFile) then throw IllegalStateException(s"could not find regular file: $sourceFile")

      val regex = new Regex(raw"""(?x)(?s) case \s+ " (?<candidate> \w+ ) " \s+ =>""")

      val candidates = regex.findAllMatchIn(Files.readString(sourceFile)).map(m => m.group("candidate")).toList

      if !candidates.contains(selection)
      then
        println(s"invalid selection: »$selection«, the following are available:")
        candidates.foreach(println)
      else
        val target = targetFolder.resolve("GeneratedSelection.scala")

        Files.writeString(
          target,
          s"""package reactives.scheduler
             |object GeneratedSelection {
             |  inline val selection: "$selection" = "$selection"
             |}
             |""".stripMargin,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING
        )
    .printHelp()
}
