package lore.cli

import java.nio.file.Path
import com.monovore.decline._
import cats.implicits._

private val langname: String = "LoRe"
private val cmdname: String = "lore"

// command line options
sealed trait OutputOptions
case class ToFile(
    toFile: Path
) extends OutputOptions
case class SplitMode(
    outDir: Path
) extends OutputOptions
case object StdOut extends OutputOptions

case class Options(
    file: Option[Path] = None, // path to a file
    inline: Option[String] = None, // inline sourcecode of a program
    outputOptions: OutputOptions = StdOut
    // toFile: Option[Path] = None,
    // splitMode: Option[Path] = None // write output to multiple separate files
):
  val toFile: Option[Path] = outputOptions match
    case ToFile(toFile) => Some(toFile)
    case _              => None
  val splitMode: Option[Path] = outputOptions match
    case SplitMode(outDir) => Some(outDir)
    case _                 => None

// ways to pass a program
val file: Opts[Path] = Opts.argument[Path](metavar = "FILE")
val inline: Opts[String] = Opts.option[String](
  "inline",
  metavar = "SOURCECODE",
  help = "Pass a program via inline sourcecode."
)
val inputOpts: Opts[Options] = (file orElse inline).map {
  case path: Path   => Options(file = Some(path))
  case prog: String => Options(inline = Some(prog))
}

// write location if output file is requested
val toFile: Opts[ToFile] = Opts
  .option[Path](
    "output",
    short = "o",
    metavar = "FILE",
    help = "Write output to a file."
  )
  .map(ToFile(_))

// subcommands
sealed trait Subcommand { val options: Options }

case class Parse(options: Options) extends Subcommand
val parseSC = Opts
  .subcommand("parse", help = s"Parse a $langname program and print the AST.") {
    inputOpts
  }
  .map(Parse(_))

// case class Interprete(options: Options) extends Subcommand
// val interpreteSubcommand = Opts.subcommand("run", help = "Run an fr program via an interpreter."){
//   progOpts
// }.map(Interprete(_))

case class ToRescala(options: Options) extends Subcommand
val toRescalaSC = Opts
  .subcommand("scalarize", help = s"Compile a $langname program to REScala.") {
    (inputOpts, toFile.orNone)
      .mapN {
        case (opt: Options, None)         => opt
        case (opt: Options, Some(toFile)) => opt.copy(outputOptions = toFile)
      }
  }
  .map(ToRescala(_))

// viper flags
val split: Opts[SplitMode] = Opts
  .option[Path](
    "split",
    short = "s",
    help = s"Write output to multiple separate files in a directory."
  )
  .map(SplitMode(_))

case class ToViper(options: Options) extends Subcommand
val toViperSC = Opts
  .subcommand(
    "viperize",
    help = s"Compile a $langname program to Viper IL for verification."
  ) {
    (inputOpts, (toFile orElse split).orNone)
      .mapN {
        case (opts, Some(outOpt)) => opts.copy(outputOptions = outOpt)
        case (opts, None)         => opts
      }
  }
  .map(ToViper(_))

// .map { case () => }
// .map { case (progOpts: Options, path, split) =>
//   ToViper(progOpts.copy(toFile = path, splitMode = split))
// }

// program
val mainCommand = Command(
  name = cmdname,
  header = s"Compiler and toolchain for the $langname programming language."
) {
  parseSC orElse toRescalaSC orElse toViperSC
}
