examples
========

A set of mutually independent example applications which are all runnable with the REScala library:
https://github.com/guidosalva/REScala

The examples in the "examples-reswing" project also use our RESwing library:
https://github.com/guidosalva/RESwing


examples.bouncing:
	A graphical application of a bouncing ball, with different implementations

examples.catchup:
	A graphical application using the mouse position as a reactive value

examples.clickcounter:
	A GUI application counting clicks

examples.continuous:
	The elevator application using continuous time

examples.datastructures:
	Examples illustrating reactive datastructures

examples.elevator:
	A graphical elevator application

examples.fisheye
	A graphical application illustrating an OSX-like "fisheye" effect

examples.followmouse:
	A graphical application using the mouse position as a reactive value

examples.linearspeed:

examples.meanwindow:
	An application illustrating the use of event windows

examples.path:
	A graphical application for drawing lines

examples.pong:
	A graphical application implementing a pong-game against the computer, controlled with the mouse

examples.range:
	The range example, illustrating the usage of reativity for functional dependancies

examples.smashingparticles:
	A simple graphical application illustrating particles

examples.tempconverter:
	A GUI application converting temperatures

examples.timer:
	Tests regarding the Timer functionality

examples.timeElapsing:
	Tests regarding the Timer functionality

== RESwing dependant projects ==

examples.dropdown:
	GUI example of various dropdown applications, e.g. to illustrate the "unwrap" interface function

== REAndroidThings dependant projects ==

Barometer4Android:
  a simple barometer app


# Universe
## Simplified Animal rules

	- Elements on a board, integer positions (x, y)
	- Elements move, one field at a time (tick)
	- Only one element per field allowed
	- Plant
		- has energy
		- grows over time
		- germinates area around it spawning new plants
	- Animal
		- can be herbivore or carnivore
		- can be male or female
		- can move, eat, age, die, procreate, give birth
		- actions drain or give energy
		- AI: go to the nearest food target in radius and eat it
	- Carnivore / Herbivore subclasses of Animal
		carnivore eats herbivores, can sleep
		herbivore eats plants
	- Male / Female subclasses of Animal with different behaviour
		male seeks mate
		female gives birth to child animals
	- Default setup:
		- spawn a new plant every day
		- spawn a new animal every week
