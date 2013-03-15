== Animal use case ==

= Simplified Animal rules =
	- Elements on a board, integer positions (x, y)
	- Elements move, one field at a time (tick)
	- Only one element per field allowed
	- Plant
		- has energy
	- Animal
		- can move, eat, age, die, procreate, give birth
		- AI: go to the nearest plant in radius and eat it
	- Male / Female subclasses of Animal with different behaviour
	- Default setup:
		- spawn a new plant every day
		- spawn a new animal every week

= Reimplementation Goals =
	- simplified version (reduced rules)
	- easier to read
	- less imperative, more functional, more object-oriented
	- lines-of-code stats should illustrate:
		Signals >= Events > Observers