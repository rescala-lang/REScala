---
title: JEScala
---
# JEScala
Advanced concurrency abstractions overcome the drawbacks of low-level techniques such as locks and monitors, freeing programmers that implement concurrent applications from the burden of concentrating on low-level details. However, with current approaches the coordination logic involved in complex coordination schemas is fragmented into several pieces including join patterns, data emissions triggered in different places of the application, and the application logic that implicitly creates dependencies among channels, hence indirectly among joins.

JEScala captures coordination schemas in a more expressive and modular way by leveraging a seamless integration of an advanced event system with join abstractions. JEScala is validated with case studies and provide a first performance assessment.

This is joint work with the [ASCOLA Research Group](http://www.emn.fr/z-info/ascola/)

[JEScala: modular coordination with declarative events and joins](http://dl.acm.org/citation.cfm?doid=2577080.2577082)

# Downloads

| Main Library                                        | [JEScala](http://www.stg.tu-darmstadt.de/media/st/research/jescala_folder/jescala-lib.jar)             |
| Complete Source                                     | [JEScala source](http://www.stg.tu-darmstadt.de/media/st/research/jescala_folder/jescala-srctar.gz)    |
| Grid, Up/Down, RSP (JEScala + ScalaJoins)           | [BenchMarks](http://www.stg.tu-darmstadt.de/media/st/research/jescala_folder/jescala-src-benchtar.gz)  |
| Case Studies in JEScala and Synthetic Join Language | [Case Studies](http://www.stg.tu-darmstadt.de/media/st/research/jescala_folder/jescala-src-evaltar.gz) |
| + compare with and without DSL                      |                                                                                                        |
