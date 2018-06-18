import tests.rescala.fullmv.transmitter.XShapeSerializabilityTest

object XShapeSerializability {
  def main(args: Array[String]): Unit = {
    val millis = if(args.length == 0) {
      10000
    } else {
      Integer.parseInt(args(0))
    }
    val failure = XShapeSerializabilityTest.run(millis)
    if(failure == null) {
      println("[SUCCESS] Run successfull!")
    } else {
      println("[FAILURE] " + failure)
    }
  }
}