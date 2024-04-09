package lore.cli

import com.monovore.decline.Opts
import lore.cli.OutputOptions.SplitMode
import cats.implicits._

import java.nio.file.Path


sealed trait Subcommand {
  val options: Options
}


case class Parse(options: Options) extends Subcommand

object Parse {

  val parseSourceCode: Opts[Parse] = Opts.subcommand("parse", help = s"Parse a $languageName program and print the AST.") {
    Options.inputOpts
  }.map(Parse.apply)

}

case class ToREScala(options: Options) extends Subcommand

object ToREScala {

  val toREScala: Opts[ToREScala] = Opts.subcommand("scalarize", help = s"Compile a $languageName program to REScala.") {
    (Options.inputOpts, Options.toFile.orNone)
      .mapN {
        case (opt: Options, None) => opt
        case (opt: Options, Some(toFile)) => opt.copy(outputOptions = toFile)
      }
  }.map(ToREScala.apply)

}


case class ToViper(options: Options) extends Subcommand

object ToViper {

  // viper flags
  private val split: Opts[SplitMode] = Opts.option[Path]("split", short = "s", help = s"Write output to multiple separate files in a directory.")
    .map(SplitMode.apply)

  val toViper: Opts[ToViper] = Opts.subcommand("viperize", help = s"Compile a $languageName program to Viper IL for verification.") {
    (Options.inputOpts, (Options.toFile orElse split).orNone).mapN {
      case (opts, Some(outOpt)) => opts.copy(outputOptions = outOpt)
      case (opts, None) => opts
    }
  }.map(ToViper.apply)

}

