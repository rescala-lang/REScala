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
