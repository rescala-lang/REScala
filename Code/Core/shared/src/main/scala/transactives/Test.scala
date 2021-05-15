package transactives

object TestAPI:

  import SimpleScheduler._

  type State[T] = SimpleState[T]

  class CustomSource[T](initState: SimpleState[T]) extends ReSource with Interp[T] {
    outer =>

    override type Value = T
    override protected[transactives] def state: State[Value]        = initState
    override protected[transactives] def name: ReName               = "I am a source name"
    override def interpret(v: Value): T                             = v
    override protected[transactives] def commit(base: Value): Value = base

    def makeChange(newValue: T) =
      new InitialChange {
        override val source = outer
        override def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean = {
          if (base != newValue) {
            writeCallback(newValue)
            true
          } else false
        }
      }
  }

  class CustomDerivedString(
      initState: State[String],
      inputSource: Interp[String]
  ) extends Derived
      with Interp[String] {
    override type Value = String
    override protected[transactives] def state: State               = initState
    override protected[transactives] def name: ReName               = "I am a name"
    override protected[transactives] def commit(base: Value): Value = base

    override protected[transactives] def reevaluate(input: ReIn): Rout = {
      val sourceVal = input.dependStatic(inputSource)
      input.withValue(sourceVal + " :D")
    }

    override def interpret(v: Value): String = v
  }

object Test {

  @main def main(): Unit = {}

}
