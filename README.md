# LoRe-DSL
> An internal Scala DSL for [LoRe](https://github.com/stg-tud/LoRe), a language for local-first reactive programming with verified safety guarantees

## Usage

This is a scala 3 sbt project. To start, compile and publish the DSL plugin locally via `sbt publishPluginLocal`. This is a custom command invoking the `clean`, `compile`, `package` and `publishLocal` commands successively. Once this is done, you can import the sbt project in your editor of choice.

Afterward, you can try compiling the given examples (via e.g. `sbt sourceExamples/clean sourceExamples/compile`) to test the plugin.

If you receive errors about the [LoRe](https://github.com/stg-tud/LoRe) dependency not being found, clone its repository and publish it locally (via `sbt publishLocal`), then resume above instructions.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## Currently supported LoRe syntax

(wip)

## Examples

(wip)