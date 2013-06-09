ReSwing
=======


About
-----

ReSwing is a reactive wrapper for the Scala Swing GUI framework using the 
ReScala library. Some Swing components have been enabled to be used with 
signals and events. Signals and events are introduced by the ReScala library to 
provide means for functional reactive programming.


Design overview
---------------

The ReSwing library was designed with the following design goals in mind:

1. Integration with Scala Swing, e.g. it is possible to incrementally replace 
   Swing components by ReSwing components and mix components of both types in
   the same GUI.
2. Coherent class hierarchy, i.e. the ReSwing class hierarchy maps to the
   original Swing class hierarchy, e.g. keeps an is-a relation between the
   class `ReLabel` and `ReComponent`, among others.

To accomplish both goals, the ReSwing class hierarchy exists side-by-side with 
the Swing class hierarchy. For every Swing component `Comp`, which a reactive 
wrapper exists for, there is a ReSwing component `ReComp`. Every ReSwing 
component holds a reference to the underlying Swing component (this is similar 
to how Scala Swing wraps around Java Swing) and can be cast *implicitly* to the 
corresponding Swing component. This enables a ReSwing component to be used in 
any place where the corresponding Swing component is expected.


Reactive values
---------------

The reactive values the component depends on, like signals and events, are set 
just once when the component is created and it is not intended to re-assign 
them later. That could be done by passing all reactive values to the 
constructor, but the ReSwing library implements a different scheme where 
defining reactive values is realized by sub-classing and overriding the base 
class values. By doing this, it is not necessary to pass the values of a base 
through all the constructors of every sub-class. The reactive values are just 
available to be modified by overriding them. But still every instantiable class 
has a companion object that allows for a constructor-like object construction 
syntax for convenience.

While the kind of reactive values described in the previous paragraph are set 
by the application developer to trigger changes in the ReSwing library, there 
are also reactive values that are only to be read by the application and are 
updated by the ReSwing library to inform the application about changes e.g. due 
to user interaction.

There is a third kind of reactive values that can be changed by the application 
as well as by the user. In the ReSwing library these values can be set to a 
signal and every change to the signal will cause the library to update the user 
interface accordingly. When accessing the signal to get the value, every change 
from the application-set signal as well as from the library (e.g. user 
interaction) is reflected.
