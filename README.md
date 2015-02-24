# fork notes

this fork adds compatibility for scala 2.11.5 as well as newer versions of REScala.
note that this needs a checkout of rescala in `../REScala` to compile.
this should be improved when the used version of rescala becomes public,
but i will probably forget …

i also kinda ragedeleted most of the original code and only kept the console signal version

# Simplified Animal rules

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
