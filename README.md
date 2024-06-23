# Chat

For TUDa members (with TU-ID) you should be able to directly join:
https://element.matrix.tu-darmstadt.de/#/room/#difosys:matrix.tu-darmstadt.de

Everyone else may join using any matrix connection:
https://matrix.to/#/#difosys:matrix.tu-darmstadt.de

# This Repository

This repository contains research software related to Programming Foundations for Distributed Systems at the [Software Technology Group](http://www.stg.tu-darmstadt.de/).

The two most likely modules in this project you are interested in are the library for reactive programming, and the library for replicated data types, both are detailed below.

All code in this repository is written in Scala and organized as a single [sbt](https://www.scala-sbt.org/) project containing multiple modules.

The simplest way to get started is to install [coursier](https://get-coursier.io/docs/cli-installation) – a single binary called `cs` – and then run `cs launch sbt` in the project root directory. This provides you with the sbt shell, where you can type `compile` or `test` to ensure that everything is working correctly. If you get strange errors you may be using a too new/old java version, try `cs launch --jvm=21 sbt` to force the use of Java 21 (will be downloaded).

> [!NOTE]
> Some JS tests require the `jsdom` npm package to be installed. Install it via `npm install jsdom` if you run into errors.

Type `projects` into the sbt shell to get an overview of the available modules. Many modules have a JVM/JS/Native/Aggregate suffix, indicating that this module is cross compiled to different backends. You can select a specific module in the sbt shell by running, for example, `project reactivesJVM`. Then the `compile` or `test` commands will only happen in the selected module. See below for an overview of the modules.

## IDE Setup

There are two IDE choices for Scala (and generally, the use of an IDE is recommended):

* IntelliJ with the Scala plugin: https://www.jetbrains.com/help/idea/get-started-with-scala.html
* Metals (a language server): https://scalameta.org/metals/

Generally, we recommend starting sbt from the command line (see above) first, and then using IntelliJ (with the Scala plugin) to open the project folder (the root folder, not any of the submodules). This should allow you to select `bsp` as the import type (instead of sbt). Using [bsp](https://www.scala-lang.org/blog/2020/10/27/bsp-in-sbt.html) means that IntelliJ is communicating directly with sbt to send commands to compile/test/run your code, providing the best compatibility.
Note that Scala support int IntelliJ is not perfect, it is possible that the IDE believes some correct code to not compile, or does not flag some errors. Fall back to trying the sbt command line directly if you have issues.
See https://youtrack.jetbrains.com/issue/SCL-12945/correctly-handle-cross-projects for a list of crossproject related issuess.

Metals also allows using [sbt as a bsp server](https://scalameta.org/metals/docs/build-tools/sbt/#sbt-build-server), but we don’t have much experience if this is necessary over the default import.


# Reactives / REScala

REScala is a set of libraries for principled reactive programming,
a programming paradigm that integrates the strong guarantees of functional reactive programming into object oriented programs,
and is useful in many contexts such as traditional user interfaces, concurrent and distributed applications, web development, and server software.

* Homepage: <http://www.rescala-lang.com/>
* Usage Manual: <http://www.rescala-lang.com/manual>
* Maven Artifacts: https://index.scala-lang.org/rescala-lang/rescala/artifacts/rescala?pre-releases=false

# Replicated Data Types (ARDTs)

The RDT library is still being actively researched

* primary paper introducing the concept: https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2023.14
* Maven Artifacts: https://index.scala-lang.org/rescala-lang/rescala/artifacts/kofre?pre-releases=false


# Repository Module Overview

Projects are within the `Modules` folder, notable ones include:

• Aead: A wrapper around authenticated encryption with associated data providers for JS & JVM
• Channels: A minimal send/receive abstraction over various data communication methods (TCP, Websockets, WebRTC, BroadcastChannel)
• Examples: Various case studies
• Lore: Invariant based coordination reasoning
• RDTs: Replicated data types, includes both automatic generation of required typeclass for algebraic types, and multiple pre-defined types
• Reactives: Time-changing values in a transactional dataflow graph.
• Replication: Combining channels, rdts, and reactives into a out-of-the-box usable framework (kinda).


# Workflow and Git

We research the core libraries in this repository. Experiments that involve significant changes to APIs and behavior are expected. Then there are experiments that branch of the core libraries requiring updates on changes to the core – these are often done by students not as familiar with the core libraries.

The monorepo aids both goals. Core changes are applied (often automatically by IDE tooling) and tested against all branch experiments. Branch experiments can immediately make use of improvements of core libraries. Moving features from branches to the core is also straightforward.

To make this work best in the git repo:
• Push/pull to main/master often. Applying core changes on merge is more painful than as part of the change.
• Ensure that at least the tip commits you push compile and test – that is, the newest public commit on main/master should always compile and test. It’s a nice bonus if intermediate commits do so as well, but not as important.
• Avoid force pushing to main/master if someone may depend on that commit (i.e., it’s fine to force push to fixup something you pushed just now, but otherwise just push a fixup commit).
• If in doubt, when working on some larger change that may require up/downstream dependencies to remain stable for a while, use the chat above for coordination.


It’s generally up to you how you want to work with git, but a couple of tips:
• “feature branches” seem to be a bad idea in our context, they tend to get lost and never merged, and cause lots of unnecessary work. Avoid.
	• Do use branches if you want to propose a change for review (but don’t let it diverge too much)
• Prefer rebasing of your own commits on top of main/master before pushing to reduce history graph complexity. Note that this is a specific choice for our project, also see the following for interesting insights: https://www.yarchive.net/comp/linux/git_rebase.html
	• Merge commits may be preferable when there are actual conflicts to be solved.


