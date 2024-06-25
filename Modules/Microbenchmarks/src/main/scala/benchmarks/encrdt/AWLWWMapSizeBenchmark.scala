package benchmarks.encrdt

object AWLWWMapSizeBenchmark {
  def main(args: Array[String]): Unit = {
    for numberElements <- List(10, 100, 1000, 10_000, 100_000) do {
      val state = new SerializeOnlyBenchmarkState()
      state.crdtSizeInElements = numberElements
      state.setupCrdtState()
      val sizeInKiB = (state.serialPlaintextState.length + state.serialPlaintextVectorClock.length).toDouble / 1024
      if sizeInKiB > 1024 then {
        println("size(" + numberElements + " elements)= " + sizeInKiB + " KiB = " + sizeInKiB / 1024 + " MiB")
      } else {
        println("size(" + numberElements + " elements)\t= " + sizeInKiB + " KiB")
      }
    }
  }
}
