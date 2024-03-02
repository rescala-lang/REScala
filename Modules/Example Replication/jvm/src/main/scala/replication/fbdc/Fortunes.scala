package replication.fbdc

import de.rmgk.script.{process, runOutput}
import rdts.datatypes.LastWriterWins


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
      dataManager.modRes { reqqI =>
        val reqq = reqqI.mutable
        fortunes.foreach { q =>
          val resp = processFortune(q.value)
          reqq.update(using dataManager.replicaId)(
            "fortune",
            Some(LastWriterWins.now(resp))
          )
        }
        reqq.result
      }
    }

  def processFortune(r: Req.Fortune) =
    Res.Fortune(r, process"fortune".runOutput())

}
