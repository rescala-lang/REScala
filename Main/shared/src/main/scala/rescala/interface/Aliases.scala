package rescala.interface

import rescala.core.Struct
import rescala.reactives

trait Aliases[S <: Struct] {
  /** Signals represent time changing values of type A
    * @group reactive */
  final type Signal[+A] = reactives.Signal[A, S]
  /** Events represent discrete occurrences of values of type A
    * @group reactive */
  final type Event[+A] = reactives.Event[A, S]
  /** @group reactive */
  final type Observe = reactives.Observe[S]
  /** @group reactive */
  final type Var[A] = reactives.Var[A, S]
  /** @group reactive */
  final type Evt[A] = reactives.Evt[A, S]
  /** @group internal */
  final type StaticTicket = rescala.core.StaticTicket[S]
  /** @group internal */
  final type DynamicTicket = rescala.core.DynamicTicket[S]
  /** @group internal */
  final type AdmissionTicket = rescala.core.AdmissionTicket[S]
  /** @group internal */
  final type AccessTicket = rescala.core.AccessTicket[S]
  /** @group internal */
  final type CreationTicket = rescala.core.CreationTicket[S]
  /** @group internal */
  final type Creation = rescala.core.Initializer[S]
  /** @group internal */
  final type Reactive = rescala.core.Reactive[S]
  /** @group internal */
  final type ReSource = rescala.core.ReSource[S]
}
