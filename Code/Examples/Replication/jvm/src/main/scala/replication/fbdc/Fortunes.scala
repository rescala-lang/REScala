package replication.fbdc

import de.rmgk.script.extensions
import kofre.datatypes.LastWriterWins

object Fortunes {

  def enableConditional(exampleData: FbdcExampleData) = {
    if process"which fortune".scriptStart().waitFor() == 0
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
      dataManager.transform { current =>
        current.modRes { reqq =>
          fortunes.foreach { q =>
            val resp = processFortune(q.value)
            reqq.observeRemoveMap.insert("fortune", Some(LastWriterWins.now(resp, exampleData.replicaId)))
          }
        }
      }
    }

  def processFortune(r: Req.Fortune) =
    Res.Fortune(r, process"fortune".run())

}
