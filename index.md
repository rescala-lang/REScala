---
title: Home
nav: 1
---

# The REScala Project


The REScala Project aims to bring the principles of functional reactive programming to the JVM.
FRP can do more than providing asynchronous collections of events with a rich API of stream transformations,
we also automatically manage consistent up-to-date state with minimal syntactic overhead,
and integrate seamlessly into modern, object-oriented and functional, concurrent, distributed programs on the JVM.

<!-- {% include slideshow.html %} -->

<br />

## Functional
{: class="info-box"}
With abstractions for Events and Signals to handle interactions and state, and seamless conversions between them.

## Consistent
{: class="info-box"}
No temporary inconsistencies, no data races, define logical constraints and derivations, and they will always be satisfied.

## Concurrent
{: class="info-box"}
Use reactive abstractions from any thread in any order, add reactives, remove them, change their connections, and it still just works.

<br />

Software applications must react to external changes such as the input from the user and network messages.
Traditionally, object-oriented software adopts the Observer pattern to implement reactivity and decouple the observers from the observables.
Whereas researchers have highlighted the drawbacks of this style for a long time, alternatives struggle to be widely accepted.
In particular, functional reactive programming and dataflow programming – which aim to represent time-changing values as first class abstractions – are promising,
but hardly escape the functional setting. On the other hand, event-based languages directly support events but do not achieve the declarative style of more functional approaches.


REScala is a reactive language which integrates concepts from event-based and functional-reactive programming into the object-oriented world.
REScala supports the development of reactive applications by fostering a functional and declarative style which complements the advantages of object-oriented design.

---
[Get Started](./manual){: class="btn btn-primary"}
[Sourcecode](https://github.com/guidosalva/REScala/){: class="btn btn-primary"}

<!-- <a class="github-button" href="https://github.com/guidosalva/REScala/archive/master.zip" data-style="mega" aria-label="Download guidosalva/REScala on GitHub">Download</a> -->



---
REScala is a research project held at TU Darmstadt in the
[Software Technology Group.](http://www.stg.tu-darmstadt.de/)
