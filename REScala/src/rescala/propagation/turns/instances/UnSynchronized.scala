package rescala.propagation.turns.instances

object UnSynchronized extends AbstractTurnFactory[Synchronized](() => new Synchronized())
