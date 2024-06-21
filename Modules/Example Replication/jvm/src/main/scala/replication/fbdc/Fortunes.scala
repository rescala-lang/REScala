package replication.fbdc

import de.rmgk.script.{process, runOutput}
import rdts.datatypes.LastWriterWins
import rdts.syntax.DeltaBuffer

object Fortunes {

  def enableConditional(exampleData: FbdcExampleData) = {
    if process"which fortune".inheritIO().start().waitFor() == 0
    then
      println(s"enabling fortunes")
      enableFortuneProcessing(exampleData)
    else
      println(s"fortunes not installed")
  }

  def enableFortuneProcessing(exampleData: FbdcExampleData) =
    import exampleData.dataManager
    exampleData.addCapability("fortune")

    exampleData.requestsOf[Req.Fortune].observe { fortunes =>
      println(s"observed fortune")
      dataManager.modRes { reqqI =>
        println(s"modifying fortune")
        val reqq = DeltaBuffer(reqqI).mutable
        fortunes.foreach { q =>
          val resp = processFortune(q.value)
          reqq.mod(_.update(using dataManager.replicaId)(
            "fortune",
            Some(LastWriterWins.now(resp))
          ))
        }
        reqq.result.state
      }
    }

  def processFortune(r: Req.Fortune) =
    Res.Fortune(r, process"fortune".runOutput())

}
