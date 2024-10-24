package rdts.datatypes.experiments.protocols

import rdts.base.Uid

case class Participants(members: Set[Uid])

object Participants:
  def participants(using p: Participants): Set[Uid] =
    p.members
