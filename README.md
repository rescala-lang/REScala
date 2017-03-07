# REScala

REScala is a set of libraries for principled reactive programming,
a programming paradigm that integrates the strong guarantees of functional reactive programming into object oriented programs,
and is useful in many contexts such as traditional user interfaces, concurrent and distributed applications, web development, and server software.

* Homepage: <http://www.rescala-lang.com/>
* Usage Manual: <http://www.rescala-lang.com/manual/>

REScala is a research project held at TU Darmstadt in the
[Software Technology Group.](http://www.stg.tu-darmstadt.de/)
In context of the [PACE Project](http://www.pace-erc.eu/).

## Repository Overview

This repository contains mutliple subprojects, the most important ones are:

* __Main__: contains the main reactive programming library, for the JVM and JS.
  Also includes a concurrent scheduler for the JVM, making it thread safe.
* __Extensions/RESwing__: contains an integration with scalaswing to write UIs on the JVM.
* __Extensions/Rescalatags__: contains an integration with scalatags to write web applications.
* __Research/Microbenchmarks__: contains a benchmark suite for reactive programming.
  Execute wit `jmh:run`
* __Tests__: integration tests for various subprojects.
* __Examples__: various example applications.
