package lore

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import com.monovore.decline._
import cats.implicits._

private val langname: String = "LoRe"
private val cmdname: String = "lore"

// command line options
case class Options(
    file: Option[Path] = None, // path to a file
    inline: Option[String] = None, // inline sourcecode of a program
    toFile: Option[Path] = None
)

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
val toFile: Opts[Path] = Opts.option[Path](
  "output",
  short = "o",
  metavar = "FILE",
  help = "Write output to a file."
)

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
    (inputOpts, toFile.map(Some(_)) withDefault None).tupled
  }
  .map { case (progOpts: Options, path) =>
    ToRescala(progOpts.copy(toFile = path))
  }

case class ToViper(options: Options) extends Subcommand
val toViperSC = Opts
  .subcommand(
    "viperize",
    help = s"Compile a $langname program to Viper IL for verification."
  ) {
    (inputOpts, toFile.map(Some(_)) withDefault None).tupled
  }
  .map { case (progOpts: Options, path) =>
    ToViper(progOpts.copy(toFile = path))
  }

// program
val mainCommand = Command(
  name = cmdname,
  header = s"Compiler and toolchain for the $langname programming language."
) {
  parseSC orElse toRescalaSC orElse toViperSC
}
