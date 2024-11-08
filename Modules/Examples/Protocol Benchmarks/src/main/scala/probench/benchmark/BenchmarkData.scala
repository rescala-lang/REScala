package probench.benchmark

case class BenchmarkData(
    name: String,
    op: String,
    args: String,
    sendTime: Long,
    receiveTime: Long,
    latency: Double,
    unit: String
)

object BenchmarkData {
  val header: Seq[String] = Seq("name", "op", "args", "send-time", "receive-time", "latency", "unit")
}
