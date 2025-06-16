# Development of this repo has moved

This repository has moved to https://github.com/stg-tud/Bismuth

REScala started as a reactive programming implementation for Scala, but evolved to focus on distributed and replicated systems research. This repository contains a snapshot of the core REScala project to preserve history. For current development, please use the new repository above which contains the full REScala lineage.


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

This repository contains multiple subprojects in the __Modules__ folder:

* __Example EncryptedTodoFx__
* __Example Misc 2015__: various example applications.
  * __examples__: misc examples involving swing, time, datastructures (no reswing, using swing directly)
    * __demo__: application design demonstrating, showing multiple steps of adding features to a reactive application
  * __reswing__: misc reswing examples
    * __millgame__: mill game using reswing
    * __reader__: imperative/reactive versions of a reswing RSS reader (actual network request, no support for failures yet)
    * __reshapes__: reswing drawing application
    * __texteditor__: basic text editor in different implementation styles (imperative, to full reactive)
  * __universe__: console application demoing parallel execution of simulations
* __Example Todolist__: Rescalatags todo application in the style of common todo applications
* __Graph-Compiler__
* __Javafx__
* __Microbenchmarks__: contains a benchmark suite for reactive programming.
  Execute with `jmh:run`
* __Reactives__: contains the main reactive programming library, for the JVM and JS.
  Also includes a concurrent scheduler for the JVM, making it thread safe.
* __Swing__: contains an integration with scalaswing to write UIs on the JVM.

The __Historical__ folder contains code that is no longer maintained:

* __Android/REAndroidThings__: contains an integration with androidthings to write apps for the android of things platform.
* __dividiParoli__
  * __DividiApp__: money dividing application, showing the use of distributed CRDTs integrated into rescala
  * __ParoliChatApp__: console application demoing distributed CRDTs
