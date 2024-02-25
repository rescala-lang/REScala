package lore.dsl

trait Invariant {

  def apply(): Boolean
  
  val representation: String

}

object Invariant {
  inline def apply(name: String)(using invariantManager: InvariantManager): Invariant = {
    val invariant = new Invariant {

      override def apply(): Boolean = {
        println(f"Checking invariant $name")

        true
      }

      override val representation: String = name

    }

    invariantManager.registerInvariant(invariant)

    invariant
  }
}

trait InvariantManager {

  def registerInvariant(invariant: Invariant): Unit

  def registeredInvariants: Seq[Invariant]

  def checkInvariants(): Boolean = {
    var success = true

    for inv <- registeredInvariants do {
      if !inv() then {
        success = false
        println(s"Interaction violated invariant: ${inv.representation} evaluated to false!")
      }
    }

    success

  }

}

given defaultInvariantManager: InvariantManager = new InvariantManager {
  private var invariants: List[Invariant] = List.empty

  override def registerInvariant(invariant: Invariant): Unit = invariants :+= invariant

  override def registeredInvariants: Seq[Invariant] = invariants
}

