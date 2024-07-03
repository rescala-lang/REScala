package lofi_acl.example.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.Filter
import lofi_acl.ardt.datatypes.LWW.filter
import lofi_acl.ardt.datatypes.ORMap.stringKeyORMapFilter
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.HasDots

type Title = String
given Bottom[Title] = Bottom.provide("")

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: ObserveRemoveMap[String, LastWriterWins[String]],
    expenses: ObserveRemoveMap[String, Expense]
) derives Lattice, Bottom, Filter

case class Expense(
    // time: LastWriterWins[LocalDateTime], // For what do we need time?
    description: LastWriterWins[Option[String]],
    amount: LastWriterWins[Option[Float]],
    comments: ObserveRemoveMap[String, LastWriterWins[String]]
) derives Lattice, HasDots, Bottom, Filter

object TravelPlan {
  given jsonCodec: JsonValueCodec[TravelPlan] = JsonCodecMaker.make[TravelPlan]
}
