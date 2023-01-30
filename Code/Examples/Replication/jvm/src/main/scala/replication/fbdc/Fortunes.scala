package replication.fbdc

import de.rmgk.script.extensions

object Fortunes {

  def enableConditional(exampleData: FbdcExampleData) = {
    if process"which fortune".runResult().isRight
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
      val resps = fortunes.map(processFortune)
      dataManager.transform { current =>
        current.modRes { reqq =>
          resps.foreach(reqq.enqueue)
        }
      }
    }

  def processFortune(r: Req.Fortune) =
    Res.Fortune(r, process"fortune".run())

}
