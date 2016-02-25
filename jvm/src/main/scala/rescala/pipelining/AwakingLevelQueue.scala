package rescala.pipelining

import rescala.graph.Reactive
import rescala.pipelining.propagation.PipelineQueue

private[pipelining] class AwakingLevelQueue(currentTurn: PipeliningTurn) extends PipelineQueue()(currentTurn) {

  @volatile
  private var needsAwake = false

  def awakeOnNewElement() = this.synchronized {
    if (isEmpty) {
      needsAwake = true
    } else {
      currentTurn.needContinue()
    }
  }

  override def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[PipelineStruct.type]): Unit = this.synchronized {
    super.enqueue(minLevel, needsEvaluate)(dep)
    if (needsAwake) {
      currentTurn.needContinue()
      needsAwake = false
    }
  }

}