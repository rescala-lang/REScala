# LoRe Compiler Plugin
> A Scala compiler plugin for compiling LoRe code written in Scala into plain LoRe.

## Usage

This is a scala 3 sbt project. To start, compile and publish the compiler plugin locally via `sbt clean;compile;package;publishLocal`. Once this is done, you can import the sbt project in your editor of choice.

Afterward, you can try compiling the given examples via `sbt loreCompilerPluginExamples/clean loreCompilerPluginExamples/compile` to test the plugin. The plugin will verbosely output logs describing the definitions it is processing to the console, but does not yet do anything else with these values.

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

- The `sourceExamples` file contains around 50 example definitions using various syntax with Sources across integers, strings and booleans
