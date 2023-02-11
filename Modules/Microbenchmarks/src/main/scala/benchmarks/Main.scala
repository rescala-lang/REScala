package benchmarks

import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder

object Main {
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder().jvmArgs(args: _*)
//      .include(".*")
//      .param("engineName", "parrp")
//      .jvmArgsAppend("-Djmh.stack.lines=1")
//      .jvmArgsAppend("-Djmh.stack.period=1")
//      //.jvmArgsAppend("-Djmh.stack.detailLine=true")
//      //.addProfiler(classOf[StackProfiler])
      .build()
    new Runner(opt).run()
    ()
  }
}
