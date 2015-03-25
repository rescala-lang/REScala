package object rescala {
  /** renaming for compatibility reasons */
  @deprecated(message = "use VarSynt instead of StaticVar", since = "unknown")
  type StaticVar[T] = VarSynt[T]
}
