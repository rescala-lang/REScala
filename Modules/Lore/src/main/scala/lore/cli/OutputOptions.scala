package lore.cli

import java.nio.file.Path

sealed trait OutputOptions

object OutputOptions {

  case class ToFile(toFile: Path) extends OutputOptions

  case class SplitMode(outDir: Path) extends OutputOptions

  case object StdOut extends OutputOptions

}
