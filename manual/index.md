---
title: Introduction
version: 0.3
number: 1
---
# REScala Reference Manual

## Introduction

This manual covers the main features of the *REScala* programming language.
{% include internal_link.html ref="sigAndVars" %} presents time-changing values
in *REScala*, {% include internal_link.html ref="events" %} describes events,
{% include internal_link.html ref="conv-fun" %} covers the conversion functions between
events and time-changing values, {% include internal_link.html ref="technicalities" %}
presents technical details that are necessary to correctly run
*REScala*, {% include internal_link.html ref="related" %} outlines the related work.


{% include paragraph.md text="Intended audience and prerequisites" %} This manuscript is
mainly intended for students who approach reactive programming in
Scala for the first time.  The manual assumes basic knowledge of the
Scala~\cite{scala} language and of functional programming (high-order
functions, anonymous functions, etc.). No previous knowledge of
reactive programming is assumed.

While a major aspect of *REScala*'s design is the integration of events
and signals, they can be used separately. For example a programmer can
use only *REScala* events to design application that do not need
time-changing values.

{% include paragraph.md text="Scope" %} The manual covers the basic features of
*REScala*. Some functionalities, including implicit events and
high-order signals are intentionally not covered, other, like event
polymorphism, are only sketched. More details can be found
in~\cite{rescala,Gasiunas:2011:EME:1960275.1960303}.

The manual introduces the concepts related to functional reactive
programming and event-based programming from a practical
perspective. The readers interested in a more general presentation of
these topics can find in {% include internal_link.html ref="related" %} the essential
references.