---
title: Signals and Vars
version: 0.3
number: 2
permalink: /manual/SignalAndVars
---
## Signals and Vars

A signal is language concept for expressing functional dependencies
among values in a declarative way. Intuitively, a reactive value can
depend on variables -- sources of change without further dependencies
-- or on other reactive values.  When any of the dependency sources
changes, the expression defining the reactive value is automatically
recomputed by the language runtime to keep the reactive value
up-to-date.

Consider the following example:

\begin{codenv}
var a = 2
var b = 3
var c = a + b   (*@\label{sum}@*)
println(a,b,c) // -> (2,3,5)
a = 4
println(a,b,c) // -> (2,4,5) (*@\label{nomore}@*)
c = a + b (*@\label{update}@*)
println(a,b,c) // -> (4,3,7)
\end{codenv}

Line~\ref{sum} specifies the value of \code{c} as a function of
\code{a} and \code{b}. Since Line~\ref{sum} defines a {\it statement},
the relation $c = a + b$ is valid after the execution of
Line~\ref{sum}. Clearly, when the value of \code{a} is updated, the
relation $c = a + b$ is not valid anymore (Line~\ref{nomore}). To make
sure that the relation still holds, the programmer needs to recompute
the expression and reassign \code{c}, like in line \ref{update}.
\\

Reactive programming and *REScala* provide abstractions to express {\em
  constraints} in addition to statements. In *REScala*, the programmer
can specify that the constraint $c := a + b$ {\em always} holds during
the execution of a program. Every time \code{a} or \code{b} change,
the value of \code{c} is automatically recomputed.

For example:
\begin{codenv}
val a = Var(2)
val b = Var(3)
val c = Signal{ a() + b() }   (*@\label{sumS}@*)
println(a.get,b.get,c.get) // -> (2,3,5)
a()= 4  (*@\label{updated}@*)
println(a.get,b.get,c.get) // -> (4,3,7)
\end{codenv}

In the code above, the signal in Line~\ref{sumS} defines the
constraint $c := a + b$. When one of the reactive values involved in
the constraint is updated (Line~\ref{updated}), the expression in the
constraint is recomputed behind the scenes, and the value of \code{a}
is automatically updated.

As the reader may have noticed, expressing constraints in *REScala*
requires to conform some syntactic conventions which are discussed in
the next sections.
