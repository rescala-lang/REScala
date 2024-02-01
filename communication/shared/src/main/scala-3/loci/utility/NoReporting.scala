package loci
package utility

import java.io.{OutputStream, PrintStream}
import java.util.Locale
import scala.quoted.*

object noReporting:
  def apply[T](default: T)(body: Quotes ?=> T)(using Quotes) =
    val reset = try
      val context = quotes.getClass.getMethod("ctx").invoke(quotes)

      val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
      val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
      val freshContextClass = Class.forName("dotty.tools.dotc.core.Contexts$FreshContext")
      val reporterClass = Class.forName("dotty.tools.dotc.reporting.Reporter")
      val noReporterClass = Class.forName("dotty.tools.dotc.reporting.Reporter$NoReporter$")

      val quotesImpl = quotesImplClass.getMethod("apply", contextClass)
      val reporter = contextClass.getMethod("reporter")
      val setReporter = freshContextClass.getMethod("setReporter", reporterClass)

      val noReporter = noReporterClass.getField("MODULE$").get(noReporterClass)

      val (freshContext, wrapUp) =
        try
          val contextsClass = Class.forName("dotty.tools.dotc.core.Contexts")
          val exploreCtx = contextsClass.getMethod("inline$exploreCtx", contextClass)
          val wrapUpExplore = contextsClass.getMethod("inline$wrapUpExplore", contextClass)

          exploreCtx.invoke(null, context) -> { wrapUpExplore.invoke(null, _) }

        catch case _: ClassNotFoundException | _: NoSuchMethodException |  _: IllegalArgumentException =>
          val contextStateClass = Class.forName("dotty.tools.dotc.core.Contexts$ContextState")
          val contextPoolClass = Class.forName("dotty.tools.dotc.core.Contexts$ContextPool")
          val exploreContextPool = contextStateClass.getMethod("exploreContextPool")
          val base = contextClass.getMethod("base")
          val next = contextPoolClass.getMethod("next", contextClass)
          val free = contextPoolClass.getMethod("free")

          val pool = exploreContextPool.invoke(base.invoke(context))
          next.invoke(pool, context) -> { (_: Any) => free.invoke(pool) }

      val exploringReporter = reporter.invoke(freshContext)
      setReporter.invoke(freshContext, noReporter)

      quotesImpl.invoke(null, freshContext) match
        case quotes: Quotes => Some((quotes, setReporter, exploringReporter, freshContext, wrapUp))
        case _ => None

    catch
      case _: ClassNotFoundException | _: NoSuchMethodException | _: NoSuchFieldException |  _: IllegalArgumentException =>
        None

    reset.fold(default) { (quotes, setReporter, exploringReporter, freshContext, wrapUp) =>
      try
        val filteredOut = new ProxyPrintStream(Console.out) {
          def filterOut(s: String) = s startsWith "exception"

          override def println(x: String) =
            if !filterOut(x) then super.println(x)

          override def println(x: Any) = x match
            case s: String if filterOut(s) =>
            case _ => super.println(x)
        }

        Console.withOut(filteredOut) {
          body(using quotes)
        }

      finally
        try
          setReporter.invoke(freshContext, exploringReporter)
          wrapUp(Seq(freshContext))
        catch
          case _: IllegalArgumentException =>
    }
  end apply

  private class NullOutputStream extends OutputStream:
    override def write(i: Int): Unit = ()

  private class ProxyPrintStream(stream: PrintStream) extends PrintStream(new NullOutputStream):
    override def flush() = stream.flush()
    override def close() = stream.close()
    override def checkError(): Boolean = stream.checkError()
    override def write(b: Int) = stream.write(b)
    override def write(buf: Array[Byte], off: Int, len: Int) = stream.write(buf, off, len)
    override def print(b: Boolean) = stream.print(b)
    override def print(c: Char) = stream.print(c)
    override def print(i: Int) = stream.print(i)
    override def print(l: Long) = stream.print(l)
    override def print(f: Float) = stream.print(f)
    override def print(d: Double) = stream.print(d)
    override def print(s: Array[Char]) = stream.print(s)
    override def print(s: String) = stream.print(s)
    override def print(obj: Any) = stream.print(obj)
    override def println() = stream.println()
    override def println(x: Boolean) = stream.println(x)
    override def println(x: Char) = stream.println(x)
    override def println(x: Int) = stream.println(x)
    override def println(x: Long) = stream.println(x)
    override def println(x: Float) = stream.println(x)
    override def println(x: Double) = stream.println(x)
    override def println(x: Array[Char]) = stream.println(x)
    override def println(x: String) = stream.println(x)
    override def println(x: Any) = stream.println(x)
    override def printf(format: String, args: Object*) = { stream.printf(format, args); this }
    override def printf(l: Locale, format: String, args: Object*) = { stream.printf(l, format, args); this }
    override def format(format: String, args: Object*) = { stream.format(format, args); this }
    override def format(l: Locale, format: String, args: Object*) = { stream.format(l, format, args); this }
    override def append(csq: CharSequence) = { stream.append(csq); this }
    override def append(csq: CharSequence, start: Int, end: Int) = { stream.append(csq, start, end); this }
    override def append(c: Char) = { stream.append(c); this }
    override def write(b: Array[Byte]) = stream.write(b)
  end ProxyPrintStream
end noReporting
