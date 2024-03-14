# LoRe-DSL
> An internal Scala DSL for [LoRe](https://github.com/stg-tud/LoRe), a language for local-first reactive programming with verified safety guarantees

## Usage

This is a scala 3 sbt project. To start, compile and publish the DSL plugin locally via `sbt publishPluginLocal`. This is a custom command invoking the `clean`, `compile`, `package` and `publishLocal` commands successively. Once this is done, you can import the sbt project in your editor of choice.

Afterward, you can try compiling the given examples (via e.g. `sbt sourceExamples/clean sourceExamples/compile`) to test the plugin. The plugin will verbosely output logs of the definitions it is processing to the console.

If you receive errors about the [LoRe](https://github.com/stg-tud/LoRe) dependency not being found, clone its repository and publish it locally (via `sbt publishLocal`), then resume above instructions.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## Currently supported LoRe syntax

- Definitions of integers (Int), strings (String) and booleans (Boolean) using literals, references and below expressions
 - Arithmetic operations `+`, `-`, `/`, `*` and parentheses on integers
 - String literals without operations (e.g. "foo")
 - The unary operation `!` and the binary operations `&&` and `||` as well as `<`, `>`, `<=`, `>=`, `==` and `!=` on booleans
- Definitions of Sources using all of the above as input values

## Examples

- The `sourceExamples` example contains around 50 example definitions using various syntax with Sources across integers, strings and booleans