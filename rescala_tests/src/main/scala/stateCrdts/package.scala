package object stateCrdts {
  type Identifier = String
  type Removed = Boolean

  def sleep: Unit = Thread sleep 2000
}