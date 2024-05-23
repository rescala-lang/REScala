# Important Notice

> [!IMPORTANT]
> Development for this compiler plugin moved to a [repo under the STG at TU Darmstadt](https://github.com/stg-tud/LoRe-compiler-plugin).
> For now, this repository will stay as is and the old readme is kept below.

# LoRe-DSL
> An internal Scala DSL for [LoRe](https://github.com/stg-tud/LoRe), a language for local-first reactive programming with verified safety guarantees

## Usage

This is a scala 3 sbt project. To start, compile and publish the DSL plugin locally via `sbt publishPluginLocal`. This is a custom command invoking the `clean`, `compile`, `package` and `publishLocal` commands successively. Once this is done, you can import the sbt project in your editor of choice.

Afterward, you can try compiling the given examples (via e.g. `sbt sourceExamples/clean sourceExamples/compile`) to test the plugin. The plugin will verbosely output logs describing the definitions it is processing to the console, but does not yet do anything else with these values.

If you receive errors about the [LoRe](https://github.com/stg-tud/LoRe) dependency not being found, clone its repository and publish it locally (via `sbt publishLocal`), then resume above instructions.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

## Currently supported (LoRe) syntax

- Definitions of integers (Int), strings (String) and booleans (Boolean) using literals, references and below expressions
  - Arithmetic operators `+`, `-`, `/`, `*` and parentheses on integers
  - String literals without operations (e.g. "foo")
  - The unary operator `!` (negation) and the binary operators `&&`, `||` as well as `<`, `>`, `<=`, `>=`, `==` and `!=`
- Definitions of Sources using all of the above as input values

## (LoRe) Syntax not supported yet (non-exhaustive)

- Definitions of Derived instances
- Definitions of Interaction instances
  - "requires", "modifies" and "executes" method calls
- Function calls
- Field calls (normal and curly braces)
- Parentheses (?)
- Sequences
- Arrow (anonymous) functions
- Type Aliases
- Asserts and Assumes (Viper)
- Invariants
- If-Clauses
- Tuples
- Implication and Bi-Implication operators
- In-Set operator
- Quantification operators (Existential and Universal)

## Examples

- The `sourceExamples` example contains around 50 example definitions using various syntax with Sources across integers, strings and booleans