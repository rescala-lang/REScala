# REScala

REScala is a set of libraries for principled reactive programming,
a programming paradigm that integrates the strong guarantees of functional reactive programming into object oriented programs,
and is useful in many contexts such as traditional user interfaces, concurrent and distributed applications, web development, and server software.

* Homepage: <http://www.rescala-lang.com/>
* Usage Manual: <http://www.rescala-lang.com/manual>

REScala is a research project held at TU Darmstadt in the
[Software Technology Group](http://www.stg.tu-darmstadt.de/)
in the context of the [PACE](http://www.pace-erc.eu/)
and [NICER](http://nicer.network/) projects.

## Repository Overview

This repository contains multiple subprojects in the __Modules__ folder, the most important ones are:

* __Main__: contains the main reactive programming library, for the JVM and JS.
  Also includes a concurrent scheduler for the JVM, making it thread safe.
* __Extensions/RESwing__: contains an integration with scalaswing to write UIs on the JVM.
* __Extensions/Rescalatags__: contains an integration with scalatags to write web applications.
* __Microbenchmarks__: contains a benchmark suite for reactive programming.
  Execute with `jmh:run`
* __Tests__: integration tests for various subprojects.
* __Examples__: various example applications.
  * __Editor__: basic text editor in different implementation styles (imperative, to full reactive)
  * __examples__: misc examples involing swing, time, datastructures (no reswing, using swing directly)
  * __examples-reswing__: misc reswing examples
  * __Mill__: mill game using reswing
  * __PongDemo__: application design demonstrating, showing multiple steps of adding features to a reactive application
  * __RSSReader__: imperative/reactive versions of a reswing RSS reader (actual network request, no support for failures yet)
  * __Shapes__: reswing drawing application
  * __Todolist__: Rescalatags todo application in the style of common todo applications
  * __Universe__: console application demoing parallel execution of simulations

The __Historical__ folder contains code that is no longer maintained:

* __Android/REAndroidThings__: contains an integration with androidthings to write apps for the android of things platform.
* __dividiParoli__
  * __DividiApp__: money dividing application, showing the use of distributed CRDTs integrated into rescala
  * __ParoliChatApp__: console application demoing distributed CRDTs
