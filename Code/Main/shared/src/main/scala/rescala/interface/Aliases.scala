package rescala.interface

import rescala.core.Struct

trait Aliases[S <: Struct] {
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
  final type Reactive = rescala.core.Derived[S]
  /** @group internal */
  final type ReSource = rescala.core.ReSource[S]
  /** @group internal */
  final type REStructure = S
}
