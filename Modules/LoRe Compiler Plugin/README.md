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
- Type instantiations e.g. List, Map (i.e. "Foo(bar)" where Foo is a type)
  - The arrow syntax for Map tuples (i.e. foo -> bar) is not supported, but normal tuples (foo, bar) are
- Arrow (anonymous) functions
- Tuples
- Definitions of Sources using all of the above as input values
- Definitions of Derived instances
- Definitions of Interaction instances
  - "requires", "modifies" and "executes" method calls
- Function calls
- Field calls (normal and curly braces)

## (LoRe) Syntax not supported yet (non-exhaustive)

- Parentheses (?)
- Sequences
- Type Aliases
- Asserts and Assumes (Viper)
- Invariants
- If-Clauses
- Implication and Bi-Implication operators
- In-Set operator
- Quantification operators (Existential and Universal)

## Examples

- The `sourceExamples` file contains around 50 example definitions using various syntax with Sources across integers, strings and booleans
- The `derivedExamples` file contains around 17 example definitions for function/method/property calls and Derived instantiations
- The `interactionExamples` file contains around 14 definitions for types (List, Map), arrow functions, and Interactions with various method calls on them
