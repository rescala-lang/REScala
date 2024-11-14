package probench.clients

import probench.benchmark.BenchmarkData
import probench.data.KVOperation
import rdts.base.Uid

import scala.collection.mutable

trait Client(name: Uid) {

  var doBenchmark: Boolean                             = false
  val benchmarkData: mutable.ListBuffer[BenchmarkData] = mutable.ListBuffer.empty

  def read(key: String): Unit                 = handleOp(KVOperation.Read(key))
  def write(key: String, value: String): Unit = handleOp(KVOperation.Write(key, value))

  def multiget(key: String, times: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do read(key.replace("%n", i.toString))
    println(s"Did $times get queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def multiput(key: String, value: String, times: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do write(key.replace("%n", i.toString), value.replace("%n", i.toString))
    println(s"Did $times put queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def mixed(min: Int, max: Int, times: Int = 1): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do {
      val num = Math.round(Math.random() * (max - min) + min).toInt
      if Math.random() > 0.5 then
        read(f"key$num")
      else
        write(f"key$num",f"value$num")
    }
    println(s"Did $times mixed queries in ${(System.nanoTime() - start) / 1_000_000}ms")
  }

  def handleOp(op: KVOperation[String, String]): Unit = {
    val start = if doBenchmark then System.nanoTime() else 0

    handleOpImpl(op)

    if doBenchmark then {
      val end = System.nanoTime()
      val opString = op match
        case KVOperation.Read(_)     => "get"
        case KVOperation.Write(_, _) => "put"
      val args = op match
        case KVOperation.Read(key)         => key
        case KVOperation.Write(key, value) => s"$key $value"
      benchmarkData.append(BenchmarkData(
        name.delegate,
        opString,
        args,
        start / 1000,
        end / 1000,
        (end - start).toDouble / 1000,
        "µs"
      ))
    }
  }

  def handleOpImpl(op: KVOperation[String, String]): Unit

}
