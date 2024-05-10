package lore.cli

import com.monovore.decline.*

private val languageName: String = "LoRe"
private val commandName: String = "lore"

val mainCommand: Command[Subcommand] = Command(
  name = commandName,
  header = s"Compiler and toolchain for the $languageName programming language."
) {
  Parse.parseSourceCode orElse ToREScala.toREScala orElse ToViper.toViper
}
