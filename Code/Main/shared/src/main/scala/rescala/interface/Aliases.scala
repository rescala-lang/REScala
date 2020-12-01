package rescala.interface

import rescala.core.Struct
import rescala.operator

trait Aliases[S <: Struct] {

  /** Signals represent time changing values of type A
    * @group reactive
    */
  final type Signal[+A] = operator.Signal[A, S]

  /** Events represent discrete occurrences of values of type A
    * @group reactive
    */
  final type Event[+A] = operator.Event[A, S]

  /** @group reactive */
  final type Observe = operator.Observe[S]

  /** @group reactive */
  final type Var[A] = operator.Var[A, S]

  /** @group reactive */
  final type Evt[A] = operator.Evt[A, S]

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
  final type Initializer = rescala.core.Initializer[S]

  /** @group internal */
  final type Derived = rescala.core.Derived[S]

  /** @group internal */
  final type ReSource = rescala.core.ReSource[S]

  /** @group internal */
  final type ReStructure = S
}
