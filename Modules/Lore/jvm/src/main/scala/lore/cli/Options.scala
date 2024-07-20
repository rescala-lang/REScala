package lore.cli

import com.monovore.decline.Opts
import lore.cli.OutputOptions.{SplitMode, StdOut, ToFile}

import java.nio.file.Path

/** @param file          Path to a file
  * @param inline        inline sourcecode of a program
  * @param outputOptions output mode. Can be either [[lore.cli.OutputOptions.ToFile]], [[lore.cli.OutputOptions.SplitMode]] or [[lore.cli.OutputOptions.StdOut]]
  */
case class Options(file: Option[Path] = None, inline: Option[String] = None, outputOptions: OutputOptions = StdOut) {

  val toFile: Option[Path] = outputOptions match {
    case ToFile(path) => Some(path)
    case _            => None
  }

  val splitMode: Option[Path] = outputOptions match {
    case SplitMode(path) => Some(path)
    case _               => None
  }

}

object Options {

  private val file: Opts[Path] = Opts.argument[Path](metavar = "FILE")
  private val inline: Opts[String] =
    Opts.option[String]("inline", metavar = "SOURCECODE", help = "Pass a program via inline sourcecode.")

  val inputOpts: Opts[Options] = (file orElse inline).map {
    case path: Path   => Options(file = Some(path))
    case prog: String => Options(inline = Some(prog))
  }

  val toFile: Opts[ToFile] = Opts.option[Path](
    "output",
    short = "o",
    metavar = "FILE",
    help = "Write output to a file."
  ).map(ToFile.apply)

}
