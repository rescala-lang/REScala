package rescala.pipelining

import rescala.pipelining.propagation.PipelineQueue
import rescala.graph.Reactive

class AwakingLevelQueue(currentTurn: PipeliningTurn) extends PipelineQueue()(currentTurn) {

  @volatile
  private var needsAwake = false

  def awakeOnNewElement() = this.synchronized {
    if (isEmpty) {
      needsAwake = true
    } else {
      currentTurn.needContinue()
    }
  }

  override def enqueue(minLevel: Int, needsEvaluate: Boolean = true)(dep: Reactive[PipelineSpores.type]): Unit = this.synchronized {
    super.enqueue(minLevel, needsEvaluate)(dep)
    if (needsAwake) {
      currentTurn.needContinue()
      needsAwake = false
    }
  }

}