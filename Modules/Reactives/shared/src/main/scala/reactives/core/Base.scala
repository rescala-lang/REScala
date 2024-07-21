package reactives.core

/** Base implementation for reactives, with [[Derived]] for scheduling,
  * together with a [[ReInfo]] and containing a [[State]]
  *
  * @param state the state passed by the scheduler
  * @param info  the name of the reactive, useful for debugging as it often contains positional information
  */
abstract class Base[V](
    override protected[reactives] val state: reactives.SelectedScheduler.State[V],
    override val info: ReInfo
) extends ReSource {

  override type State[V2] = reactives.SelectedScheduler.State[V2]
  override type Value    = V
  override def toString: String = s"${info.description}($state)"
}
