package rescala.meta

import rescala.graph.{Reactive, ReevaluationResult, Struct}
import rescala.propagation.Turn

/**
  * Created by nico on 24/07/2016.
  */
/*class ManagedReactive[S <: Struct](element: ReactiveElement[S], evalFun : () => ReevaluationResult[S] = () => ReevaluationResult.Static[S](changed = false)) extends Reactive[S] {
  /**
    * Spore that is used to internally manage the reactive evaluation of this value
    *
    * @return Spore for this value
    */
  override protected[rescala] def bud: S#Spore[Reactive[S]] = element.bud

  /**
    * Reevaluates this value when it is internally scheduled for reevaluation
    *
    * @param turn Turn that handles the reevaluation
    * @return Result of the reevaluation
    */
  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = evalFun()
}
*/