#!/usr/bin/env rt-scala-runner
//> using scala 3.4.0
//> using dep de.rmgk.slips::options:0.7.0
//> using dep de.rmgk.slips::script:0.8.0
import java.nio.file.Path
import java.nio.file.Files
import de.rmgk.options
import java.nio.file.StandardOpenOption
import de.rmgk.script.*

object SelectScheduler {
  def main(args: Array[String]): Unit =
    options.parseArguments(args.toList):
      val selection = options
        .positional("scheduler", "default scheduler for all", "default")
        .value

      val scheduler = selection match {
        case "fullmv"   => "reactives.fullmv.FullMVUtil.defaultScheduler"
        case "parrp"    => "reactives.parrp.ParRPDefault.scheduler"
        case "toposort" => "reactives.scheduler.TopoBundle.TopoScheduler"
        case "sidup"    => "reactives.scheduler.SynchronizedSidup.scheduler"
        case "calculus" => "reactives.scheduler.CalculusLike.FScheduler"
        case _          => "reactives.scheduler.LevelbasedVariants.synchron"
      }
      val target = Path.of(
        "Modules/Reactives/shared/src/main/scala/reactives/scheduler/GeneratedSelection.scala"
      )
      Files.writeString(
        target,
        s"package reactives.generated { object Scheduler { val selection = $scheduler } }",
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      val p = process"cs launch scalafmt -- $target".runInherit()
}
